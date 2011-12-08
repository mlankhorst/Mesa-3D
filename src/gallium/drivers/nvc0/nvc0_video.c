/*
 * Copyright 2011 Maarten Lankhorst
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#define NVC0_USE_AUTOBIND
#include "vl/vl_decoder.h"
#include "vl/vl_video_buffer.h"
#include "vl/vl_types.h"

#include "nvc0_context.h"
#include "nvc0_screen.h"
#include "nvc0_video.h"

#include "util/u_video.h"
#include "util/u_sampler.h"

#define debug_printf(x...) fprintf(stderr, x);

extern uint64_t
nouveau_bo_gpu_address(struct nouveau_channel *chan,
		       struct nouveau_bo *bo);

// Sorry, I can't redistribute these
static const uint32_t fw_mpeg12_vuc[] = {
#include "nvc0-vd-uc-mpeg12.h"
};

static const uint32_t fw_mpeg4_vuc[] = {
#if 1
#include "nvc0-vd-uc-mpeg4.h"
#else // Same, but vp3t differs..
#include "nvc0-vd-uc-mpeg4-alt.h"
#endif
};

static const uint32_t fw_vc1_vuc[] = {
#include "nvc0-vd-uc-vc1.h"
};

static const uint32_t fw_h264_vuc[] = {
#include "nvc0-vd-uc-h264.h"
};

int
nvc0_screen_get_video_param(struct pipe_screen *pscreen,
                            enum pipe_video_profile profile,
                            enum pipe_video_cap param)
{
   switch (param) {
   case PIPE_VIDEO_CAP_SUPPORTED:
      return profile != PIPE_VIDEO_PROFILE_UNKNOWN;
   case PIPE_VIDEO_CAP_NPOT_TEXTURES:
      return 1;
   case PIPE_VIDEO_CAP_MAX_WIDTH:
   case PIPE_VIDEO_CAP_MAX_HEIGHT:
      return vl_video_buffer_max_size(pscreen);
   default:
      debug_printf("unknown video param: %d\n", param);
      return 0;
   }
}

static uint32_t nvc0_video_align(uint32_t h) {
   return ((h+0x3f)&~0x3f);
};

static uint32_t mb(uint32_t coord) {
   return (coord + 0xf)>>4;
}

static uint32_t mb_half(uint32_t coord) {
   return (coord + 0x1f)>>5;
}

#define SLICE_SIZE 0x200
#define COMM_OFFSET 0x800

struct nvc0_video_buffer {
   struct pipe_video_buffer base;
   unsigned num_planes, valid_ref;
   struct pipe_resource *resources[4];
   struct pipe_sampler_view *sampler_view_planes[6];
};

struct nvc0_decoder {
   struct pipe_video_decoder base;
   struct nouveau_grobj *bsp, *vp, *ppp;
   struct nouveau_bo *fence_bo, *bsp_bo, *inter_bo, *fw_bo;
   unsigned *fence_map;
   struct comm *comm;

   // array size max_references + 1, contains unpostprocessed images
   struct nouveau_bo *ref_bo;

   // tmp is an array for h264, with each member being used for a ref frame or current
   // target.. size = (((mb(w)*((mb(h)+1)&~1))+3)>>2)<<8 * (max_references+1)
   // for other codecs, it simply seems that size = w*h is enough
   // unsure what it's supposed to contain..
   struct nouveau_bo *tmp_bo;
   struct {
      struct nvc0_video_buffer *vidbuf;
      unsigned last_used;
      unsigned h264_info; // nfi what to put here yet..
   } refs[17];
   unsigned fence_seq, fw_sizes, last_frame_num, tmp_stride, ref_stride;
};

static INLINE void
nvc0_decoder_ycbcr_offsets(struct nvc0_decoder *dec, uint32_t *y2, uint32_t *cbcr, uint32_t *cbcr2)
{
   uint32_t w = mb(dec->base.width), size;
   *y2 = mb_half(dec->base.height)*w;
   *cbcr = *y2 * 2;
   *cbcr2 = *cbcr + w * (nvc0_video_align(dec->base.height)>>6);

   /* The check here should never fail because it means a bug
    * in the code rather than a bug in hardware..
    */
   size = (2 * (*cbcr2 - *cbcr) + *cbcr) << 8;
   if (size > dec->ref_stride) {
      debug_printf("Overshot ref_stride (%u) with size %u and ofs (%u,%u,%u)\n",
                   dec->ref_stride, size, *y2<<8, *cbcr<<8, *cbcr2<<8);
      *y2 = *cbcr = *cbcr2 = 0;
      assert(size <= dec->ref_stride);
   }
}

static INLINE void
nvc0_decoder_inter_sizes(struct nvc0_decoder *dec, uint32_t slice_count,
                         uint32_t *slice_size, uint32_t *bucket_size, uint32_t *ring_size)
{
   *slice_size = (SLICE_SIZE * slice_count)>>8;
   if (u_reduce_video_profile(dec->base.profile) == PIPE_VIDEO_CODEC_MPEG12)
      *bucket_size = 0;
   else
      *bucket_size = mb(dec->base.width) * 3;
   *ring_size = (dec->inter_bo->size >> 8) - *bucket_size - *slice_size;
}

static void
nvc0_decoder_handle_references(struct nvc0_decoder *dec, struct nvc0_video_buffer *refs[16], unsigned seq, struct nvc0_video_buffer *target)
{
   unsigned i, empty_spot = dec->base.max_references + 1;
   for (i = 0; i < dec->base.max_references; ++i) {
      unsigned idx;
      if (!refs[i])
         continue;

      idx = refs[i]->valid_ref;
      //debug_printf("ref[%i] %p in slot %i\n", i, refs[i], idx);
      assert(target != refs[i]);
      assert(u_reduce_video_profile(dec->base.profile) != PIPE_VIDEO_CODEC_MPEG4_AVC ||
             dec->refs[idx].last_used == seq - 1);

      if (dec->refs[idx].vidbuf != refs[i]) {
         debug_printf("%p is not a real ref\n", refs[i]);
         // FIXME: Maybe do m2mf copy here if a application really depends on it?
         continue;
      }

      assert(dec->refs[idx].vidbuf == refs[i]);
      dec->refs[idx].last_used = seq;
   }
   /* Try to find a real empty spot first, there should be one..
    */
   for (i = 0; i < dec->base.max_references + 1; ++i) {
      if (dec->refs[i].last_used < seq) {
         if (!dec->refs[i].vidbuf) {
            empty_spot = i;
            break;
         }
         if (empty_spot < dec->base.max_references+1 &&
             dec->refs[empty_spot].last_used < dec->refs[i].last_used)
            continue;
         empty_spot = i;
      }
   }
   assert(empty_spot < dec->base.max_references+1);
   dec->refs[empty_spot].last_used = seq;
//   debug_printf("Kicked %p to add %p to slot %i\n", dec->refs[empty_spot].vidbuf, target, i);
   dec->refs[empty_spot].vidbuf = target;
   target->valid_ref = empty_spot;
}

static void
nvc0_decoder_kick_ref(struct nvc0_decoder *dec, struct nvc0_video_buffer *target)
{
   dec->refs[target->valid_ref].vidbuf = NULL;
   dec->refs[target->valid_ref].last_used = 0;
   target->valid_ref = 0;
//   debug_printf("Unreffed %p\n", target);
}

static uint32_t
nvc0_decoder_fill_picparm_mpeg12_bsp(struct nvc0_decoder *dec,
                                     struct pipe_mpeg12_picture_desc *desc,
                                     char *map) {
   struct mpeg12_picparm_bsp *pic_bsp = (struct mpeg12_picparm_bsp*)map;
   int i;
   pic_bsp->width = dec->base.width;
   pic_bsp->height = dec->base.height;
   pic_bsp->picture_structure = desc->picture_structure;
   pic_bsp->picture_coding_type = desc->picture_coding_type;
   pic_bsp->intra_dc_precision = desc->intra_dc_precision;
   pic_bsp->frame_pred_frame_dct = desc->frame_pred_frame_dct;
   pic_bsp->concealment_motion_vectors = desc->concealment_motion_vectors;
   pic_bsp->intra_vlc_format = desc->intra_vlc_format;
   pic_bsp->pad = 0;
   for (i = 0; i < 4; ++i)
      pic_bsp->f_code[i/2][i%2] = desc->f_code[i/2][i%2] + 1; // FU

   return (desc->num_slices << 4) | (dec->base.profile != PIPE_VIDEO_PROFILE_MPEG1);
}

static uint32_t
nvc0_decoder_fill_picparm_mpeg12_vp(struct nvc0_decoder *dec,
                                    struct pipe_mpeg12_picture_desc *desc,
                                    struct nvc0_video_buffer *refs[16],
                                    unsigned *is_ref,
                                    char *map) {
   struct mpeg12_picparm_vp pic_vp_stub = {}, *pic_vp = &pic_vp_stub;
   uint32_t i, ret = 0x01010, ring; // !async_shutdown << 16 | watchdog << 12 | irq_record << 4 | unk;
   assert(!(dec->base.width & 0xf));
   *is_ref = desc->picture_coding_type <= 2;

   if (dec->base.profile == PIPE_VIDEO_PROFILE_MPEG1)
      pic_vp->picture_structure = 3;
   else
      pic_vp->picture_structure = desc->picture_structure;

   assert(desc->picture_structure != 4);
   if (desc->picture_structure == 4) // Untested, but should work
      ret |= 0x100;
   pic_vp->width = mb(dec->base.width);
   pic_vp->height = mb(dec->base.height);
   pic_vp->unk08 = pic_vp->unk04 = (dec->base.width+0xf)&~0xf; // Stride

   nvc0_decoder_ycbcr_offsets(dec, &pic_vp->ofs[1], &pic_vp->ofs[3], &pic_vp->ofs[4]);
   pic_vp->ofs[5] = pic_vp->ofs[3];
   pic_vp->ofs[0] = pic_vp->ofs[2] = 0;
   nvc0_decoder_inter_sizes(dec, 1, &ring, &pic_vp->bucket_size, &pic_vp->inter_ring_data_size);

   assert(!desc->concealment_motion_vectors);
   pic_vp->alternate_scan = desc->alternate_scan;
   pic_vp->pad2[0] = pic_vp->pad2[1] = pic_vp->pad2[2] = 0;
   pic_vp->unk30 = desc->picture_structure < 3 && (desc->picture_structure == 2 - desc->top_field_first);
   pic_vp->unk3a = (desc->picture_coding_type == 1);
   for (i = 0; i < 4; ++i)
      pic_vp->f_code[i] = desc->f_code[i/2][i%2] + 1; // FU
   pic_vp->picture_coding_type = desc->picture_coding_type;
   pic_vp->intra_dc_precision = desc->intra_dc_precision;
   pic_vp->q_scale_type = desc->q_scale_type;
   pic_vp->top_field_first = desc->top_field_first;
   pic_vp->full_pel_forward_vector = desc->full_pel_forward_vector;
   pic_vp->full_pel_backward_vector = desc->full_pel_backward_vector;
   memcpy(pic_vp->intra_quantizer_matrix, desc->intra_matrix, 0x40);
   memcpy(pic_vp->non_intra_quantizer_matrix, desc->non_intra_matrix, 0x40);
   memcpy(map, pic_vp, sizeof(*pic_vp));
   if (desc->ref_forward) {
      refs[0] = (struct nvc0_video_buffer*)desc->ref_forward;
      refs[1] = (struct nvc0_video_buffer*)desc->ref_backward;
   } else {
      refs[0] = (struct nvc0_video_buffer*)desc->ref_backward;
   }
   return ret | (dec->base.profile != PIPE_VIDEO_PROFILE_MPEG1);
}

static uint32_t
nvc0_decoder_fill_picparm_mpeg4_bsp(struct nvc0_decoder *dec,
                                    struct pipe_mpeg4_picture_desc *desc,
                                    char *map) {
   struct mpeg4_picparm_bsp *pic_bsp = (struct mpeg4_picparm_bsp*)map;
   uint32_t t, bits = 0;
   pic_bsp->width = dec->base.width;
   pic_bsp->height = dec->base.height;
   assert(desc->vop_time_increment_resolution > 0);

   t = desc->vop_time_increment_resolution - 1;
   while (t) {
      bits++;
      t /= 2;
   }
   if (!bits)
      bits = 1;
   t = desc->vop_time_increment_resolution - 1;
   pic_bsp->vop_time_increment_size = bits;
   pic_bsp->interlaced = desc->interlaced;
   pic_bsp->resync_marker_disable = desc->resync_marker_disable;
   return 4;
}

static uint32_t
nvc0_decoder_fill_picparm_mpeg4_vp(struct nvc0_decoder *dec,
                                   struct pipe_mpeg4_picture_desc *desc,
                                   struct nvc0_video_buffer *refs[16],
                                   unsigned *is_ref,
                                   char *map) {
   struct mpeg4_picparm_vp pic_vp_stub = {}, *pic_vp = &pic_vp_stub;
   uint32_t ring, ret = 0x01014; // !async_shutdown << 16 | watchdog << 12 | irq_record << 4 | unk;
   assert(!(dec->base.width & 0xf));
   *is_ref = desc->vop_coding_type <= 1;

   pic_vp->width = dec->base.width;
   pic_vp->height = mb(dec->base.height)<<4;
   pic_vp->unk0c = pic_vp->unk08 = mb(dec->base.width)<<4; // Stride

   nvc0_decoder_ycbcr_offsets(dec, &pic_vp->ofs[1], &pic_vp->ofs[3], &pic_vp->ofs[4]);
   pic_vp->ofs[5] = pic_vp->ofs[3];
   pic_vp->ofs[0] = pic_vp->ofs[2] = 0;
   pic_vp->pad1 = pic_vp->pad2 = 0;
   nvc0_decoder_inter_sizes(dec, 1, &ring, &pic_vp->bucket_size, &pic_vp->inter_ring_data_size);

   // If you trip this assert, congratulations and help me find out what they're supposed to be
   assert(!desc->interlaced && !desc->quarter_sample && !desc->top_field_first);

   pic_vp->trd[0] = desc->trd[0];
   pic_vp->trd[1] = desc->trd[1];
   pic_vp->trb[0] = desc->trb[0];
   pic_vp->trb[1] = desc->trb[1];
   pic_vp->u48 = 0; // Codec?
   pic_vp->pad1 = pic_vp->pad2 = pic_vp->pad3 = 0;
   pic_vp->f_code_fw = desc->vop_fcode_forward;
   pic_vp->f_code_bw = desc->vop_fcode_backward;
   pic_vp->u51 = pic_vp->u52 = pic_vp->u53 = pic_vp->u54 = 0;
   pic_vp->vop_coding_type = desc->vop_coding_type;
   pic_vp->rounding_control = desc->rounding_control;
   pic_vp->alternate_vertical_scan_flag = desc->alternate_vertical_scan_flag;
   pic_vp->u58 = 0;

   memcpy(pic_vp->intra, desc->intra_matrix, 0x40);
   memcpy(pic_vp->non_intra, desc->non_intra_matrix, 0x40);
   memcpy(map, pic_vp, sizeof(*pic_vp));
   if (desc->ref_forward) {
      refs[0] = (struct nvc0_video_buffer*)desc->ref_forward;
      refs[1] = (struct nvc0_video_buffer*)desc->ref_backward;
   } else {
      refs[0] = (struct nvc0_video_buffer*)desc->ref_backward;
   }
   return ret;
}

static uint32_t
nvc0_decoder_fill_picparm_vc1_bsp(struct nvc0_decoder *dec,
                                  struct pipe_vc1_picture_desc *d,
                                  char *map) {
   struct vc1_picparm_bsp *vc = (struct vc1_picparm_bsp*)map;
   uint32_t caps = (d->slice_count << 4)&0xfff0;
   vc->width = dec->base.width;
   vc->height = dec->base.height;
   vc->profile = dec->base.profile - PIPE_VIDEO_PROFILE_VC1_SIMPLE; // 04
   vc->postprocflag = d->postprocflag;
   vc->pulldown = d->pulldown;
   vc->interlaced = d->interlace;
   vc->tfcntrflag = d->tfcntrflag; // 08
   vc->finterpflag = d->finterpflag;
   vc->psf = d->psf;
   vc->pad = 0;
   vc->multires = d->multires; // 0c
   vc->syncmarker = d->syncmarker;
   vc->rangered = d->rangered;
   vc->maxbframes = d->maxbframes;
   vc->dquant = d->dquant; // 10
   vc->panscan_flag = d->panscan_flag;
   vc->refdist_flag = d->refdist_flag;
   vc->quantizer = d->quantizer;
   vc->extended_mv = d->extended_mv; // 14
   vc->extended_dmv = d->extended_dmv;
   vc->overlap = d->overlap;
   vc->vstransform = d->vstransform;
   return caps | 2;
}

static uint32_t
nvc0_decoder_fill_picparm_h264_bsp(struct nvc0_decoder *dec,
                                   struct pipe_h264_picture_desc *d,
                                   char *map) {
   struct h264_picparm_bsp stub_h = {}, *h = &stub_h;
   uint32_t caps = (d->slice_count << 4)&0xfff0;
   assert(offsetof(struct h264_picparm_bsp, bottom_field_flag) == (0x39 + 0x24));
   h->unk00 = 1;
   h->pad1 = h->pad3 = 0;
   h->unk = 0;
   h->log2_max_frame_num_minus4 = d->log2_max_frame_num_minus4;
   h->frame_mbs_only_flag = d->frame_mbs_only_flag;
   h->direct_8x8_inference_flag = d->direct_8x8_inference_flag;
   h->width_mb = mb(dec->base.width);
   h->height_mb = mb(dec->base.height);
   h->entropy_coding_mode_flag = d->entropy_coding_mode_flag;
   h->pic_order_present_flag = d->pic_order_present_flag;
   h->pic_order_cnt_type = d->pic_order_cnt_type;
   h->log2_max_pic_order_cnt_lsb_minus4 = d->log2_max_pic_order_cnt_lsb_minus4;
   h->delta_pic_order_always_zero_flag = d->delta_pic_order_always_zero_flag;
   h->num_ref_idx_l0_active_minus1 = d->num_ref_idx_l0_active_minus1;
   h->num_ref_idx_l1_active_minus1 = d->num_ref_idx_l1_active_minus1;
   h->weighted_pred_flag = d->weighted_pred_flag;
   h->weighted_bipred_idc = d->weighted_bipred_idc;
   h->pic_init_qp_minus26 = d->pic_init_qp_minus26;
   h->deblocking_filter_control_present_flag = d->deblocking_filter_control_present_flag;
   h->redundant_pic_cnt_present_flag = d->redundant_pic_cnt_present_flag;
   h->transform_8x8_mode_flag = d->transform_8x8_mode_flag;
   h->mb_adaptive_frame_field_flag = d->mb_adaptive_frame_field_flag;
   h->field_pic_flag = d->field_pic_flag;
   h->bottom_field_flag = d->bottom_field_flag;
   memset(h->real_pad, 0, sizeof(h->real_pad));
   *(struct h264_picparm_bsp*)map = *h;
   return caps | 3;
}

static uint32_t
nvc0_decoder_fill_picparm_h264_vp(struct nvc0_decoder *dec,
                                  const struct pipe_h264_picture_desc *d,
                                  struct nvc0_video_buffer *refs[16],
                                  unsigned *is_ref,
                                  char *map) {
   struct h264_picparm_vp stub_h = {}, *h = &stub_h;
   unsigned ring, i, j = 0;
   assert(offsetof(struct h264_picparm_vp, u224) == 0x224);
   *is_ref = d->is_reference;
   assert(!d->frame_num || dec->last_frame_num + 1 == d->frame_num || dec->last_frame_num == d->frame_num);
   dec->last_frame_num = d->frame_num;

   h->width = mb(dec->base.width);
   h->height = mb(dec->base.height);
   h->stride1 = h->stride2 = mb(dec->base.width)*16;
   nvc0_decoder_ycbcr_offsets(dec, &h->ofs[1], &h->ofs[3], &h->ofs[4]);
   h->ofs[5] = h->ofs[3];
   h->ofs[0] = h->ofs[2] = 0;
   h->u24 = dec->tmp_stride >> 8;
   assert(h->u24);
   nvc0_decoder_inter_sizes(dec, 1, &ring, &h->bucket_size, &h->inter_ring_data_size);

   h->u220 = 0;
   assert(d->frame_mbs_only_flag);
   assert(!d->mb_adaptive_frame_field_flag);
   h->f0 = 0;
   h->f1 = d->direct_8x8_inference_flag;
   assert(d->direct_8x8_inference_flag == 1 && d->frame_mbs_only_flag == 1);
   h->weighted_pred_flag = d->weighted_pred_flag;
   h->f3 = 0;
   h->is_reference = d->is_reference;
   h->interlace = d->field_pic_flag;
   h->bottom_field_flag = d->bottom_field_flag;
   h->f7 = 0;
   h->log2_max_frame_num_minus4 = d->log2_max_frame_num_minus4;
   h->u31_45 = d->frame_mbs_only_flag;
   assert(h->u31_45 == 1);

   h->pic_order_cnt_type = d->pic_order_cnt_type;
   h->pic_init_qp_minus26 = d->pic_init_qp_minus26;
   h->chroma_qp_index_offset = d->chroma_qp_index_offset;
   h->second_chroma_qp_index_offset = d->second_chroma_qp_index_offset;
   h->weighted_bipred_idc = d->weighted_bipred_idc; // TODO confirm..
   h->tmp_idx = 0; // set in h264_vp_refs below
   h->fifo_dec_index = 2; // BECAUSE I CAN!
   h->frame_number = d->frame_num;
   h->u34_3030 = h->u34_3131 = 0;
   h->field_order_cnt[0] = d->field_order_cnt[0];
   h->field_order_cnt[1] = d->field_order_cnt[1];
   memset(h->refs, 0, sizeof(h->refs));
   memcpy(h->m4x4, d->scaling_lists_4x4, sizeof(h->m4x4) + sizeof(h->m8x8));
   h->u220 = 0;
   for (i = 0; i < dec->base.max_references; ++i) {
      if (!d->refs[i].surface)
         continue;
      refs[j] = (struct nvc0_video_buffer*)d->refs[i].surface;
      h->refs[j].fifo_idx = j + (j >= 2);
      h->refs[j].tmp_idx = refs[j]->valid_ref;
      h->refs[j].field_order_cnt[0] = d->refs[i].field_order_cnt[0];
      h->refs[j].field_order_cnt[1] = d->refs[i].field_order_cnt[1];
      h->refs[j].frame_idx = d->refs[i].frame_idx;
      h->refs[j].unk12 = d->refs[i].top_is_reference;
      h->refs[j].unk13 = d->refs[i].bottom_is_reference;
      h->refs[j].unk14 = 0;
      h->refs[j].notseenyet = 0;
      h->refs[j].unk16 = 0;
      h->refs[j].unk17 = 1;
      h->refs[j].unk21 = 1;
      h->refs[j].pad = 0;
      assert(!d->refs[i].is_long_term);
      j++;
   }
   *(struct h264_picparm_vp*)map = *h;
   return 0x1113;
}

static void
nvc0_decoder_fill_picparm_h264_vp_refs(struct nvc0_decoder *dec,
                                       struct pipe_h264_picture_desc *d,
                                       struct nvc0_video_buffer *refs[16],
                                       struct nvc0_video_buffer *target,
                                       char *map) {
   struct h264_picparm_vp *h = (struct h264_picparm_vp*)map;
   assert(dec->refs[target->valid_ref].vidbuf == target);
//    debug_printf("Target: %p\n", target);

   h->tmp_idx = target->valid_ref;
}

static uint32_t
nvc0_decoder_fill_picparm_vc1_vp(struct nvc0_decoder *dec,
                                 struct pipe_vc1_picture_desc *d,
                                 struct nvc0_video_buffer *refs[16],
                                 unsigned *is_ref,
                                 char *map) {
   struct vc1_picparm_vp *vc = (struct vc1_picparm_vp*)map;
   unsigned ring;
   assert(dec->base.profile != PIPE_VIDEO_PROFILE_VC1_SIMPLE);
   *is_ref = d->picture_type <= 1;

   nvc0_decoder_ycbcr_offsets(dec, &vc->ofs[1], &vc->ofs[3], &vc->ofs[4]);
   vc->ofs[5] = vc->ofs[3];
   vc->ofs[0] = vc->ofs[2] = 0;
   vc->width = dec->base.width;
   vc->height = mb(dec->base.height)<<4;
   vc->unk0c = vc->unk10 = mb(dec->base.width)<<4; // Stride
   vc->pad = vc->pad2 = 0;
   nvc0_decoder_inter_sizes(dec, 1, &ring, &vc->bucket_size, &vc->inter_ring_data_size);
   vc->profile = dec->base.profile - PIPE_VIDEO_PROFILE_VC1_SIMPLE;
   vc->loopfilter = d->loopfilter;
   vc->u32 = 0;
   vc->dquant = d->dquant;
   vc->overlap = d->overlap;
   vc->quantizer = d->quantizer;
   vc->u36 = 0; // fastuvmc?
   // Paranoid settings, nfi what triggers the remaining unknowns yet
   assert(!d->multires && !d->rangered && !d->extended_mv && !d->psf && !d->interlace && !d->fastuvmc);
   if (d->ref_forward) {
      refs[0] = (struct nvc0_video_buffer*)d->ref_forward;
      refs[1] = (struct nvc0_video_buffer*)d->ref_backward;
   } else {
      refs[0] = (struct nvc0_video_buffer*)d->ref_backward;
   }
   return 0x12;
}

static void dump_comm_bsp(struct comm *comm) {
#if 0
   unsigned idx = comm->bsp_cur_index & 0xf;
   debug_printf("Cur seq: %x, bsp byte ofs: %x\n", comm->bsp_cur_index, comm->byte_ofs);
   debug_printf("Status: %08x, pos: %08x\n", comm->status[idx], comm->pos[idx]);
#endif
}

static void dump_comm_vp(struct nvc0_decoder *dec, struct comm *comm, unsigned slice_size) {
#if 0
	unsigned i, idx = comm->pvp_cur_index & 0xf;
	debug_printf("Status: %08x, stage: %08x\n", comm->status_vp[idx], comm->pvp_stage);
	debug_printf("Acked byte ofs: %x, bsp byte ofs: %x\n", comm->acked_byte_ofs, comm->byte_ofs);
	debug_printf("Irq/parse indexes: %i %i\n", comm->irq_index, comm->parse_endpos_index);

	for (i = 0; i != comm->irq_index; ++i)
		debug_printf("irq[%i] = { @ %08x -> %04x }\n", i, comm->irq_pos[i], comm->irq_470[i]);
	for (i = 0; i != comm->parse_endpos_index; ++i)
		debug_printf("parse_endpos[%i] = { @ %08x -> %04x }\n", i, comm->parse_endpos[i], comm->parse_endpos[i]);
	if (comm->status_vp[idx] == 1)
		return;

	if (0 && (comm->pvp_stage & 0xff) != 0xff) {
		unsigned i, *map;
		assert(nouveau_bo_map(dec->inter_bo, NOUVEAU_BO_RD) >= 0);
		map = dec->inter_bo->map;
		for (i = 0; i < comm->byte_ofs + slice_size; i += 0x10) {
			debug_printf("%05x: %08x %08x %08x %08x\n", i, map[i/4], map[i/4+1], map[i/4+2], map[i/4+3]);
		}
		nouveau_bo_unmap(dec->inter_bo);
	}
	assert((comm->pvp_stage & 0xff) == 0xff);
#endif
}

static uint64_t
nvc0_video_addr(struct nvc0_decoder *dec, struct nvc0_video_buffer *target, struct nouveau_channel *chan, bool validate)
{
   uint64_t ret;
   if (target)
      ret = dec->ref_stride * target->valid_ref;
   else
      ret = dec->ref_stride * (dec->base.max_references+1);
   if (validate)
      nouveau_bo_validate(chan, dec->ref_bo, NOUVEAU_BO_RDWR);
   return ret + nouveau_bo_gpu_address(chan, dec->ref_bo);
}

static void
nvc0_decoder_setup_ppp(struct nvc0_decoder *dec, struct nvc0_video_buffer *target, uint32_t low700) {
   struct nouveau_screen *screen = &((struct nvc0_context*)dec->base.context)->screen->base;
   struct nouveau_channel *chan = screen->channel;
   uint32_t stride_in = mb(dec->base.width);
   uint32_t stride_out = mb(target->resources[0]->width0);
   uint32_t dec_h = mb(dec->base.height);
   uint32_t dec_w = mb(dec->base.width);
   uint64_t in_addr;
   uint32_t y2, cbcr, cbcr2, i;

   nvc0_decoder_ycbcr_offsets(dec, &y2, &cbcr, &cbcr2);

   BEGIN_RING(chan, dec->ppp, 0x700, 10);
   in_addr = nvc0_video_addr(dec, target, chan, 1) >> 8;

   OUT_RING(chan, (stride_out << 24) | (stride_out << 16) | low700); // 700
   OUT_RING(chan, (stride_in << 24) | (stride_in << 16) | (dec_h << 8) | dec_w); // 704
   assert(dec_w == stride_in);

   /* Input: */
   OUT_RING(chan, in_addr); // 708
   OUT_RING(chan, in_addr + y2); // 70c
   OUT_RING(chan, in_addr + cbcr); // 710
   OUT_RING(chan, in_addr + cbcr2); // 714

   for (i = 0; i < 4; ++i) {
      struct nv04_resource *res = nv04_resource(target->resources[i]);
      if (!res->offset)
         nouveau_bo_validate(chan, res->bo, NOUVEAU_BO_RDWR);
      in_addr = nouveau_bo_gpu_address(chan, res->bo) + res->offset;
      OUT_RING(chan, in_addr >> 8);
   }
}

static uint32_t
nvc0_decoder_vc1_ppp(struct nvc0_decoder *dec, struct pipe_vc1_picture_desc *desc, struct nvc0_video_buffer *target) {
   struct nouveau_screen *screen = &((struct nvc0_context*)dec->base.context)->screen->base;
   struct nouveau_channel *chan = screen->channel;

   nvc0_decoder_setup_ppp(dec, target, 0x1412);
   assert(!desc->deblockEnable);
   assert(!(dec->base.width & 0xf));
   assert(!(dec->base.height & 0xf));

   BEGIN_RING(chan, dec->ppp, 0x400, 1);
   OUT_RING(chan, desc->pquant << 11);

   // 728 = wtf?
   return 0x10;
}

static void
nvc0_decoder_ppp(struct nvc0_decoder *dec, union pipe_desc desc, struct nvc0_video_buffer *target, unsigned comm_seq) {
   struct nouveau_screen *screen = &((struct nvc0_context*)dec->base.context)->screen->base;
   struct nouveau_channel *chan = screen->channel;
   unsigned ppp_caps = 0x10, spin = 0;

   switch (u_reduce_video_profile(dec->base.profile)) {
   case PIPE_VIDEO_CODEC_MPEG12: {
      unsigned mpeg2 = dec->base.profile != PIPE_VIDEO_PROFILE_MPEG1;
      nvc0_decoder_setup_ppp(dec, target, 0x1410 | mpeg2);
      break;
   }
   case PIPE_VIDEO_CODEC_MPEG4: nvc0_decoder_setup_ppp(dec, target, 0x1414); break;
   case PIPE_VIDEO_CODEC_VC1: ppp_caps = nvc0_decoder_vc1_ppp(dec, desc.vc1, target); break;
   case PIPE_VIDEO_CODEC_MPEG4_AVC: nvc0_decoder_setup_ppp(dec, target, 0x1413); break;
   default: assert(0);
   }
   BEGIN_RING(chan, dec->ppp, 0x734, 2);
   OUT_RING(chan, comm_seq);
   OUT_RING(chan, ppp_caps);

   BEGIN_RING(chan, dec->ppp, 0x240, 3);
   OUT_RELOCh(chan, dec->fence_bo, 32, NOUVEAU_BO_RDWR);
   OUT_RELOCl(chan, dec->fence_bo, 32, NOUVEAU_BO_RDWR);
   OUT_RING(chan, dec->fence_seq);

   BEGIN_RING(chan, dec->ppp, 0x300, 1);
   OUT_RING(chan, 1);
   FIRE_RING(chan);

   do {
      usleep(100);
      if ((spin++ & 0xff) == 0xff)
         debug_printf("ppp%u: %u\n", dec->fence_seq, dec->fence_map[8]);
   } while (dec->fence_seq > dec->fence_map[8]);
}

static unsigned
nvc0_valid_vc1_start_marker(unsigned num_blocks,
                            unsigned total_bytes,
                            unsigned const *num_bytes,
                            const void *const *data)
{
   int j, i = 0, one = 0, zeros = 0;
   for (i = 0; i < num_blocks; ++i) {
      const char *cur = data[i];
      for (j = 0; j < num_bytes[i]; ++j) {
         if (one)
            return cur[j] == 0x0c || cur[j] == 0x0d;
         else if (cur[j] == 0x00)
            ++zeros;
         else if (zeros >= 2 && cur[j] == 0x01)
            one = 1;
         else
            return 0;
      }
   }
   return 0;
}

static void
nvc0_decoder_decode_bitstream(struct pipe_video_decoder *decoder,
                              struct pipe_video_buffer *video_target,
                              struct pipe_picture_desc *picture,
                              unsigned num_blocks, unsigned total_bytes,
                              unsigned const *num_bytes, const void *const *data)
{
   struct nvc0_decoder *dec = (struct nvc0_decoder *)decoder;
   struct nvc0_video_buffer *target = (struct nvc0_video_buffer*)video_target;
   struct nouveau_screen *screen = &((struct nvc0_context*)dec->base.context)->screen->base;
   struct nouveau_channel *chan = screen->channel;
   enum pipe_video_codec codec = u_reduce_video_profile(dec->base.profile);
   uint64_t bsp_addr, comm_addr, inter_addr, ucode_addr, pic_addr[17], last_addr, null_addr;
   uint32_t startmarker = 0, endmarker, vp_caps = 0, caps, stat, spin = 0;
   uint32_t is_ref, comm_seq = ++dec->fence_seq;
   uint32_t slice_size, bucket_size, ring_size;
   struct nvc0_video_buffer *refs[16] = {};
   char *bsp;
   struct comm *comm = dec->comm;
   int ret, i;
   struct strparm_bsp *str_bsp;
   union pipe_desc desc;
   desc.base = picture;

   ret = nouveau_bo_map_range(dec->bsp_bo, 0, ((total_bytes + 20 + 0xfff) & ~0xfff) + 0x1000, NOUVEAU_BO_INVAL|NOUVEAU_BO_WR);
   if (ret)
      goto err;
   bsp = dec->bsp_bo->map;

   switch (codec){
   case PIPE_VIDEO_CODEC_MPEG12:
      endmarker = 0xb7010000;
      caps = nvc0_decoder_fill_picparm_mpeg12_bsp(dec, desc.mpeg12, bsp);
      vp_caps = nvc0_decoder_fill_picparm_mpeg12_vp(dec, desc.mpeg12, refs, &is_ref, bsp + 0x600);
      break;
   case PIPE_VIDEO_CODEC_MPEG4:
      endmarker = 0xb1010000;
      caps = nvc0_decoder_fill_picparm_mpeg4_bsp(dec, desc.mpeg4, bsp);
      vp_caps = nvc0_decoder_fill_picparm_mpeg4_vp(dec, desc.mpeg4, refs, &is_ref, bsp + 0x600);
      break;
   case PIPE_VIDEO_CODEC_VC1: {
      if (!nvc0_valid_vc1_start_marker(num_blocks, total_bytes, num_bytes, data))
         startmarker = 0x0d010000;
      assert(dec->base.profile != PIPE_VIDEO_PROFILE_VC1_ADVANCED ||
             !startmarker || desc.vc1->frame_coding_mode != 3);
      endmarker = 0x0a010000;
      caps = nvc0_decoder_fill_picparm_vc1_bsp(dec, desc.vc1, bsp);
      vp_caps = nvc0_decoder_fill_picparm_vc1_vp(dec, desc.vc1, refs, &is_ref, bsp + 0x600);
      break;
   }
   case PIPE_VIDEO_CODEC_MPEG4_AVC: {
      endmarker = 0xb1010000;
      caps = nvc0_decoder_fill_picparm_h264_bsp(dec, desc.h264, bsp);
      vp_caps = nvc0_decoder_fill_picparm_h264_vp(dec, desc.h264, refs, &is_ref, bsp + 0x600);
      break;
   }
   default: nouveau_bo_unmap(dec->bsp_bo); assert(0); return;
   }
   nvc0_decoder_handle_references(dec, refs, dec->fence_seq, target);
   if (codec == PIPE_VIDEO_CODEC_MPEG4_AVC)
      nvc0_decoder_fill_picparm_h264_vp_refs(dec, desc.h264, refs, target, bsp + 0x600);

   caps |= 1 << 17; // enable watchdog
   bsp += 0x100;

   str_bsp = (struct strparm_bsp*)bsp;
   memset(str_bsp, 0, 0x80);
   str_bsp->w0[0] = total_bytes + 16 + 4 * !!startmarker;
   str_bsp->w1[0] = 0x1;
   bsp += 0x100;
   /* Reserved for saving/restoring BITPLANE_DATA */
   bsp += 0x400;
   /* Reserved for picparm_vp */
   bsp += 0x300;
   bsp += 0x700;
   if (startmarker) {
      *(uint32_t*)bsp = startmarker;
      bsp += 4;
   }
   for (i = 0; i < num_blocks; ++i) {
      memcpy(bsp, data[i], num_bytes[i]);
      bsp += num_bytes[i];
   }

   /* Append end sequence */
   *(uint32_t*)bsp = endmarker;
   bsp += 4;
   *(uint32_t*)bsp = 0x00000000;
   bsp += 4;
   *(uint32_t*)bsp = endmarker;
   bsp += 4;
   *(uint32_t*)bsp = 0x00000000;

   nouveau_bo_unmap(dec->bsp_bo);
   memset(comm, 0, 0x200);

   nouveau_bo_validate(chan, dec->bsp_bo, NOUVEAU_BO_RD);
   nouveau_bo_validate(chan, dec->fence_bo, NOUVEAU_BO_RDWR);
   nouveau_bo_validate(chan, dec->inter_bo, NOUVEAU_BO_RDWR);
   bsp_addr = nouveau_bo_gpu_address(chan, dec->bsp_bo) >> 8;
   comm_addr = (nouveau_bo_gpu_address(chan, dec->fence_bo) + COMM_OFFSET) >> 8;
   inter_addr = nouveau_bo_gpu_address(chan, dec->inter_bo) >> 8;

   BEGIN_RING(chan, dec->bsp, 0x700, 5);
   OUT_RING(chan, caps); // 700 cmd
   OUT_RING(chan, bsp_addr+1); // 704 strparm_bsp
   OUT_RING(chan, bsp_addr+0x10); // 708 str addr
   OUT_RING(chan, comm_addr); // 70c comm
   OUT_RING(chan, comm_seq); // 710 seq

   if (codec != PIPE_VIDEO_CODEC_MPEG4_AVC) {
      nvc0_decoder_inter_sizes(dec, 1, &slice_size, &bucket_size, &ring_size);
      BEGIN_RING(chan, dec->bsp, 0x400, 6);
      OUT_RING(chan, bsp_addr); // 400 picparm addr
      OUT_RING(chan, inter_addr); // 404 interparm addr
      OUT_RING(chan, inter_addr + slice_size + bucket_size); // 408 interdata addr
      OUT_RING(chan, ring_size << 8); // 40c interdata_size
      OUT_RING(chan, bsp_addr+2); // 410 BITPLANE_DATA
      OUT_RING(chan, 0x400); // 414 BITPLANE_DATA_SIZE
   } else {
      nvc0_decoder_inter_sizes(dec, desc.h264->slice_count, &slice_size, &bucket_size, &ring_size);
      BEGIN_RING(chan, dec->bsp, 0x400, 8);
      OUT_RING(chan, bsp_addr); // 400 picparm addr
      OUT_RING(chan, inter_addr); // 404 interparm addr
      OUT_RING(chan, slice_size << 8); // 408 interparm size?
      OUT_RING(chan, inter_addr + slice_size + bucket_size); // 40c interdata addr
      OUT_RING(chan, ring_size << 8); // 410 interdata size
      OUT_RING(chan, inter_addr + slice_size); // 414
      OUT_RING(chan, bucket_size << 8); // 418 bucket size? unshifted..
      OUT_RING(chan, 0); // 41c targets
      // TODO: Double check 414 / 418 with nvidia trace
   }

   BEGIN_RING(chan, dec->bsp, 0x240, 3);
   OUT_RELOCh(chan, dec->fence_bo, 0, NOUVEAU_BO_RDWR);
   OUT_RELOCl(chan, dec->fence_bo, 0, NOUVEAU_BO_RDWR);
   OUT_RING(chan, dec->fence_seq);

   BEGIN_RING(chan, dec->bsp, 0x300, 1);
   OUT_RING(chan, 1);
   FIRE_RING(chan);

   do {
      usleep(100);
      if ((spin++ & 0xff) == 0xff) {
         debug_printf("%u: %u\n", dec->fence_seq, dec->fence_map[0]);
         dump_comm_bsp(dec->comm);
      }
   } while (dec->fence_seq > dec->fence_map[0]);

   stat = dec->comm->status[comm_seq & 0xf];
   dump_comm_bsp(dec->comm);

   assert(stat == 2);

   if (stat != 2 || !dec->fw_sizes)
      goto end;

   BEGIN_RING(chan, dec->vp, 0x700, 7);
   pic_addr[16] = nvc0_video_addr(dec, target, chan, 1) >> 8;
   last_addr = null_addr = nvc0_video_addr(dec, NULL, chan, 0) >> 8;

   for (i = 0; i < dec->base.max_references; ++i) {
      if (!refs[i])
         pic_addr[i] = last_addr;
      else if (dec->refs[refs[i]->valid_ref].vidbuf == refs[i])
         last_addr = pic_addr[i] = nvc0_video_addr(dec, refs[i], chan, 0) >> 8;
      else
         pic_addr[i] = null_addr;
   }

   nouveau_bo_validate(chan, dec->bsp_bo, NOUVEAU_BO_RD);
   nouveau_bo_validate(chan, dec->fence_bo, NOUVEAU_BO_RDWR);
   nouveau_bo_validate(chan, dec->inter_bo, NOUVEAU_BO_RDWR);
   nouveau_bo_validate(chan, dec->fw_bo, NOUVEAU_BO_RD);
   bsp_addr = nouveau_bo_gpu_address(chan, dec->bsp_bo) >> 8;
   comm_addr = (nouveau_bo_gpu_address(chan, dec->fence_bo) + COMM_OFFSET)>>8;
   inter_addr = nouveau_bo_gpu_address(chan, dec->inter_bo) >> 8;
   ucode_addr = nouveau_bo_gpu_address(chan, dec->fw_bo) >> 8;

   OUT_RING(chan, vp_caps); // 700
   OUT_RING(chan, comm_seq); // 704
   OUT_RING(chan, 0); // 708 fuc targets, ignored for nvc0

   OUT_RING(chan, dec->fw_sizes); // 70c
   OUT_RING(chan, bsp_addr+6); // 710 picparm_addr
   OUT_RING(chan, inter_addr); // 714 inter_parm
   OUT_RING(chan, inter_addr + slice_size + bucket_size); // 718 inter_data_ofs
   if (bucket_size) {
      uint64_t tmpimg_addr;
      nouveau_bo_validate(chan, dec->tmp_bo, NOUVEAU_BO_RDWR);
      tmpimg_addr = nouveau_bo_gpu_address(chan, dec->tmp_bo)>>8;
      BEGIN_RING(chan, dec->vp, 0x71c, 2);
      OUT_RING(chan, tmpimg_addr); // 71c
      OUT_RING(chan, inter_addr + slice_size); // 720 bucket_ofs
   }
   if (codec == PIPE_VIDEO_CODEC_MPEG4_AVC) {
      int i;
      BEGIN_RING(chan, dec->vp, 0x724, 5);
      OUT_RING(chan, comm_addr); // 724
      OUT_RING(chan, ucode_addr); // 728
      OUT_RING(chan, pic_addr[0]); // 72c
      OUT_RING(chan, pic_addr[1]); // 730
      OUT_RING(chan, pic_addr[16]); // 734
      for (i = 2; i < dec->base.max_references; ++i) {
         if (!refs[i])
            break;
         assert(0x400 + (i - 2) * 4 < 0x438);
         BEGIN_RING(chan, dec->vp, 0x400 + (i-2)*4, 1);
         OUT_RING(chan, pic_addr[i]);
      }

      BEGIN_RING(chan, dec->vp, 0x438, 1);
      OUT_RING(chan, desc.h264->slice_count);
   } else {
      BEGIN_RING(chan, dec->vp, 0x724, 5);
      OUT_RING(chan, comm_addr); // 724
      OUT_RING(chan, ucode_addr); // 728
      OUT_RING(chan, pic_addr[16]); // 72c
      OUT_RING(chan, pic_addr[0]); // 730
      OUT_RING(chan, pic_addr[1]); // 734
   }

   //debug_printf("Decoding %08lx with %08lx and %08lx\n", pic_addr[16], pic_addr[0], pic_addr[1]);

   BEGIN_RING(chan, dec->vp, 0x240, 3);
   OUT_RELOCh(chan, dec->fence_bo, 16, NOUVEAU_BO_RDWR);
   OUT_RELOCl(chan, dec->fence_bo, 16, NOUVEAU_BO_RDWR);
   OUT_RING(chan, dec->fence_seq);

   BEGIN_RING(chan, dec->vp, 0x300, 1);
   OUT_RING(chan, 1);
   FIRE_RING(chan);

   spin = 0;
   do {
      usleep(100);
      if ((spin++ & 0xff) == 0xff) {
         debug_printf("vp%u: %u\n", dec->fence_seq, dec->fence_map[4]);
         dump_comm_vp(dec, dec->comm, slice_size << 8);
      }
   } while (dec->fence_seq > dec->fence_map[4]);
   dump_comm_vp(dec, dec->comm, slice_size << 8);

   nvc0_decoder_ppp(dec, desc, target, comm_seq);

end:
   if (!is_ref)
      nvc0_decoder_kick_ref(dec, target);
   return;

err:
   debug_printf("Creation failed: %s (%i)\n", strerror(-ret), ret);
   return;
}

static void
nvc0_decoder_flush(struct pipe_video_decoder *decoder)
{
   struct nvc0_decoder *dec = (struct nvc0_decoder *)decoder;
   (void)dec;
}

static void
nvc0_decoder_destroy(struct pipe_video_decoder *decoder)
{
   struct nvc0_decoder *dec = (struct nvc0_decoder*)decoder;

   nouveau_bo_ref(NULL, &dec->tmp_bo);
   nouveau_bo_ref(NULL, &dec->ref_bo);
   nouveau_bo_ref(NULL, &dec->inter_bo);
   nouveau_bo_ref(NULL, &dec->bsp_bo);
   nouveau_bo_ref(NULL, &dec->fence_bo);
   nouveau_bo_ref(NULL, &dec->fw_bo);
   nouveau_grobj_free(&dec->bsp);
   nouveau_grobj_free(&dec->vp);
   nouveau_grobj_free(&dec->ppp);
   FREE(dec);
}

struct pipe_video_decoder *
nvc0_create_decoder(struct pipe_context *context,
                    enum pipe_video_profile profile,
                    enum pipe_video_entrypoint entrypoint,
                    enum pipe_video_chroma_format chroma_format,
                    unsigned width, unsigned height, unsigned max_references)
{
   struct nouveau_screen *screen = &((struct nvc0_context*)context)->screen->base;
   struct nouveau_channel *chan = screen->channel;
   struct nvc0_decoder *dec;
   int ret;
   uint32_t codec, ppp_codec = 3;
   uint32_t timeout, y2, cbcr, cbcr2;
   char *map;

   if (getenv("XVMC_VL"))
       return vl_create_decoder(context, profile, entrypoint,
                                chroma_format, width, height,
                                max_references);

   if (entrypoint != PIPE_VIDEO_ENTRYPOINT_BITSTREAM) {
      debug_printf("%x\n", entrypoint);
      return NULL;
   }

   dec = CALLOC_STRUCT(nvc0_decoder);
   if (!dec)
      return NULL;
   ret = nouveau_grobj_alloc(chan, 0xc40090b1, 0x90b1, &dec->bsp);
   if (!ret)
      ret = nouveau_grobj_alloc(chan, 0xc40090b2, 0x90b2, &dec->vp);
   if (!ret)
      ret = nouveau_grobj_alloc(chan, 0xc40090b3, 0x90b3, &dec->ppp);
   if (ret < 0)
      goto fail;

   dec->base.context = context;
   dec->base.profile = profile;
   dec->base.entrypoint = entrypoint;
   dec->base.chroma_format = chroma_format;
   dec->base.width = width;
   dec->base.height = height;
   dec->base.max_references = max_references;
   dec->base.destroy = nvc0_decoder_destroy;
   dec->base.flush = nvc0_decoder_flush;
   dec->base.decode_bitstream = nvc0_decoder_decode_bitstream;

   ret = nouveau_bo_new(screen->device, NOUVEAU_BO_GART|NOUVEAU_BO_MAP, 0, 0x1000,
                        &dec->fence_bo);
   if (!ret)
      ret = nouveau_bo_new_tile(screen->device, NOUVEAU_BO_VRAM, 0,
                                4 << 20, 0x10, 0xfe00, &dec->bsp_bo);
   if (!ret)
      ret = nouveau_bo_new_tile(screen->device, NOUVEAU_BO_VRAM, 0x100,
                                8 << 20, 0x10, 0xfe00, &dec->inter_bo);
   if (!ret)
      ret = nouveau_bo_new(screen->device, NOUVEAU_BO_VRAM, 0,
                           0x4000, &dec->fw_bo);
   if (ret)
      goto fail;
   nouveau_bo_map(dec->fence_bo, NOUVEAU_BO_RDWR);
   dec->fence_map = dec->fence_bo->map;
   dec->fence_map[0] = dec->fence_map[4] = dec->fence_map[8] = 0;
   dec->comm = (struct comm*)(dec->fence_map + (COMM_OFFSET/sizeof(*dec->fence_map)));

   ret = nouveau_bo_map(dec->fw_bo, NOUVEAU_BO_INVAL|NOUVEAU_BO_WR);
   if (ret)
      goto fail;
   switch (u_reduce_video_profile(profile)) {
      case PIPE_VIDEO_CODEC_MPEG12: {
         codec = 1;
         memcpy(dec->fw_bo->map, fw_mpeg12_vuc, sizeof(fw_mpeg12_vuc));
         assert((sizeof(fw_mpeg12_vuc) & 0xff) == 0xe0);
         dec->fw_sizes = (0x2e0<<16) | (sizeof(fw_mpeg12_vuc) - 0x2e0);
         assert(max_references <= 2);
         break;
      }
      case PIPE_VIDEO_CODEC_MPEG4: {
         codec = 4;
         memcpy(dec->fw_bo->map, fw_mpeg4_vuc, sizeof(fw_mpeg4_vuc));
         assert((sizeof(fw_mpeg4_vuc) & 0xff) == 0xe0);
         dec->fw_sizes = (0x2e0<<16) | (sizeof(fw_mpeg4_vuc) - 0x2e0);
         assert(max_references <= 2);
         break;
      }
      case PIPE_VIDEO_CODEC_VC1: {
         ppp_codec = codec = 2;
         memcpy(dec->fw_bo->map, fw_vc1_vuc, sizeof(fw_vc1_vuc));
         assert((sizeof(fw_vc1_vuc) & 0xff) == 0xac);
         dec->fw_sizes = (0x3ac<<16) | (sizeof(fw_vc1_vuc) - 0x3ac);
         assert(max_references <= 2);
         break;
      }
      case PIPE_VIDEO_CODEC_MPEG4_AVC: {
         codec = 3;
         memcpy(dec->fw_bo->map, fw_h264_vuc, sizeof(fw_h264_vuc));
         assert((sizeof(fw_h264_vuc) & 0xff) == 0x70);
         dec->fw_sizes = (0x370<<16) | (sizeof(fw_h264_vuc) - 0x370);
         assert(max_references <= 16);
         break;
      }
      default:
         debug_printf("No support yet for other codecs, working on it!\n");
         goto fail;
   }
   nouveau_bo_unmap(dec->fw_bo);

   if (u_reduce_video_profile(profile) != PIPE_VIDEO_CODEC_MPEG12) {
      unsigned long size;
      if (u_reduce_video_profile(profile) != PIPE_VIDEO_CODEC_MPEG4_AVC) {
         size = mb(height)*16 * mb(width)*16;
      } else {
         dec->tmp_stride = 16 * mb_half(width) * nvc0_video_align(height) / 2;
         size = dec->tmp_stride * (max_references + 1);
      }
      ret = nouveau_bo_new_tile(screen->device, NOUVEAU_BO_VRAM, 0,
                                size, 0x10, 0xfe00, &dec->tmp_bo);
      if (ret)
         goto fail;
   }
   dec->ref_stride = mb(width)*16 * (mb_half(height)*32 + nvc0_video_align(height)/2);
   ret = nouveau_bo_new_tile(screen->device, NOUVEAU_BO_VRAM, 0,
                             dec->ref_stride * (max_references+2),
                             0x10, 0xfe00, &dec->ref_bo);
   if (ret)
      goto fail;

   /* Add a black reference image for when an invalid image is being requested
    * as source
    */
   ret = nouveau_bo_map_range(dec->ref_bo, dec->ref_stride * (max_references + 1),
                              dec->ref_stride, NOUVEAU_BO_INVAL|NOUVEAU_BO_WR);
   if (ret)
      goto fail;
   map = dec->ref_bo->map;
   nvc0_decoder_ycbcr_offsets(dec, &y2, &cbcr, &cbcr2);
   memset(map, 16, cbcr);
   memset(map, 128, 2 * (cbcr2 - cbcr));
   nouveau_bo_unmap(dec->ref_bo);

   timeout = 0;

   dec->bsp->grclass |= 3 << 16;
   dec->vp->grclass |= 1 << 16;
   dec->ppp->grclass |= 2 << 16;

   BEGIN_RING(chan, dec->bsp, 0x200, 2);
   OUT_RING(chan, codec);
   OUT_RING(chan, timeout);

   BEGIN_RING(chan, dec->vp, 0x200, 2);
   OUT_RING(chan, codec);
   OUT_RING(chan, timeout);

   BEGIN_RING(chan, dec->ppp, 0x200, 2);
   OUT_RING(chan, ppp_codec);
   OUT_RING(chan, timeout);

   ++dec->fence_seq;
#if 0
   /* So lets test if the fence is working? */
   BEGIN_RING(chan, dec->bsp, 0x240, 3);
   OUT_RELOCh(chan, dec->fence_bo, 0, NOUVEAU_BO_RDWR);
   OUT_RELOCl(chan, dec->fence_bo, 0, NOUVEAU_BO_RDWR);
   OUT_RING(chan, dec->fence_seq);

   BEGIN_RING(chan, dec->bsp, 0x304, 1);
   OUT_RING(chan, 1);

   BEGIN_RING(chan, dec->vp, 0x240, 3);
   OUT_RELOCh(chan, dec->fence_bo, 16, NOUVEAU_BO_RDWR);
   OUT_RELOCl(chan, dec->fence_bo, 16, NOUVEAU_BO_RDWR);
   OUT_RING(chan, dec->fence_seq);

   BEGIN_RING(chan, dec->vp, 0x304, 1);
   OUT_RING(chan, 1);

   BEGIN_RING(chan, dec->ppp, 0x240, 3);
   OUT_RELOCh(chan, dec->fence_bo, 32, NOUVEAU_BO_RDWR);
   OUT_RELOCl(chan, dec->fence_bo, 32, NOUVEAU_BO_RDWR);
   OUT_RING(chan, dec->fence_seq);

   BEGIN_RING(chan, dec->ppp, 0x304, 1);
   OUT_RING(chan, 1);
   FIRE_RING(chan);

   usleep(100);
   while (dec->fence_seq > dec->fence_map[0] &&
          dec->fence_seq > dec->fence_map[4] &&
          dec->fence_seq > dec->fence_map[8]) {
      debug_printf("%u: %u %u %u\n", dec->fence_seq, dec->fence_map[0], dec->fence_map[4], dec->fence_map[8]);
      usleep(100);
   }
   debug_printf("%u: %u %u %u\n", dec->fence_seq, dec->fence_map[0], dec->fence_map[4], dec->fence_map[8]);
#endif

   return &dec->base;

fail:
   debug_printf("Creation failed: %s (%i)\n", strerror(-ret), ret);
   nvc0_decoder_destroy(&dec->base);
   return NULL;
}

static struct pipe_sampler_view **
nvc0_video_buffer_sampler_view_planes(struct pipe_video_buffer *buffer, unsigned intrl)
{
   struct nvc0_video_buffer *buf = (struct nvc0_video_buffer *)buffer;
   if (!intrl)
      return NULL;
   else
      return buf->sampler_view_planes;
}

static void
nvc0_video_buffer_destroy(struct pipe_video_buffer *buffer)
{
   struct nvc0_video_buffer *buf = (struct nvc0_video_buffer *)buffer;
   unsigned i;

   assert(buf);

   for (i = 0; i < buf->num_planes; ++i) {
      pipe_resource_reference(&buf->resources[i], NULL);
      pipe_sampler_view_reference(&buf->sampler_view_planes[i], NULL);
   }
   FREE(buffer);
}

struct pipe_video_buffer *
nvc0_video_buffer_create(struct pipe_context *pipe,
                         enum pipe_format buffer_format,
                         enum pipe_video_chroma_format chroma_format,
                         unsigned width, unsigned height)
{
   struct nvc0_video_buffer *buffer;
   unsigned i;
   struct pipe_resource templ;
   struct pipe_sampler_view sv_templ;

   if (getenv("XVMC_VL"))
      return vl_video_buffer_create(pipe, buffer_format, chroma_format, width, height);

   assert(chroma_format == PIPE_VIDEO_CHROMA_FORMAT_420);

   buffer = CALLOC_STRUCT(nvc0_video_buffer);
   if (!buffer)
      return NULL;
   assert(!(width&1));

   buffer->base.buffer_format = PIPE_FORMAT_NV12;
   buffer->base.context = pipe;
   buffer->base.destroy = nvc0_video_buffer_destroy;
   buffer->base.chroma_format = chroma_format;
   buffer->base.width = width;
   buffer->base.height = height;
   buffer->base.get_sampler_view_planes = nvc0_video_buffer_sampler_view_planes;
   buffer->num_planes = 4;

   memset(&templ, 0, sizeof(templ));
   templ.target = PIPE_TEXTURE_2D;
   templ.depth0 = 1;
   templ.bind = PIPE_BIND_SAMPLER_VIEW | PIPE_BIND_RENDER_TARGET;
   templ.usage = PIPE_USAGE_STATIC;
   templ.format = PIPE_FORMAT_R8_UNORM;
   templ.width0 = mb(width)*16;
   templ.height0 = nvc0_video_align(height) / 2;
   templ.flags = NVC0_RESOURCE_FLAG_VIDEO;
   for (i = 0; i < 2; ++i) {
      buffer->resources[i] = pipe->screen->resource_create(pipe->screen, &templ);
      if (!buffer->resources[i])
         goto error;
   }
   templ.format = PIPE_FORMAT_R8G8_UNORM;
   templ.width0 /= 2;
   templ.height0 /= 2;
   for (; i < 4; ++i) {
      buffer->resources[i] = pipe->screen->resource_create(pipe->screen, &templ);
      if (!buffer->resources[i])
         goto error;
   }

   for (i = 0; i < buffer->num_planes; ++i ) {
      struct pipe_resource *res = buffer->resources[i];
      memset(&sv_templ, 0, sizeof(sv_templ));
      u_sampler_view_default_template(&sv_templ, res, res->format);
      sv_templ.swizzle_b = PIPE_SWIZZLE_GREEN;
      sv_templ.swizzle_g = PIPE_SWIZZLE_RED;
      buffer->sampler_view_planes[i] = pipe->create_sampler_view(pipe, res, &sv_templ);
      if (!buffer->sampler_view_planes[i])
         goto error;
   }
   return &buffer->base;

error:
   nvc0_video_buffer_destroy(&buffer->base);
   return NULL;
}
