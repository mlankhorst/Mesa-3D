/**************************************************************************
 *
 * Copyright 2010 Thomas Balling Sørensen.
 * All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sub license, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial portions
 * of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.
 * IN NO EVENT SHALL TUNGSTEN GRAPHICS AND/OR ITS SUPPLIERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 **************************************************************************/

#include "util/u_memory.h"
#include "util/u_math.h"
#include "util/u_debug.h"
#include "util/u_video.h"

#include "vdpau_private.h"

/**
 * Create a VdpDecoder.
 */
VdpStatus
vlVdpDecoderCreate(VdpDevice device,
                   VdpDecoderProfile profile,
                   uint32_t width, uint32_t height,
                   uint32_t max_references,
                   VdpDecoder *decoder)
{
   enum pipe_video_profile p_profile;
   struct pipe_context *pipe;
   struct pipe_screen *screen;
   vlVdpDevice *dev;
   vlVdpDecoder *vldecoder;
   VdpStatus ret;
   bool supported;

   VDPAU_MSG(VDPAU_TRACE, "[VDPAU] Creating decoder\n");

   if (!decoder)
      return VDP_STATUS_INVALID_POINTER;
   *decoder = 0;

   if (!(width && height))
      return VDP_STATUS_INVALID_VALUE;

   p_profile = ProfileToPipe(profile);
   if (p_profile == PIPE_VIDEO_PROFILE_UNKNOWN)
      return VDP_STATUS_INVALID_DECODER_PROFILE;

   dev = vlGetDataHTAB(device);
   if (!dev)
      return VDP_STATUS_INVALID_HANDLE;

   pipe = dev->context->pipe;
   screen = dev->vscreen->pscreen;
   supported = screen->get_video_param
   (
      screen,
      p_profile,
      PIPE_VIDEO_CAP_SUPPORTED
   );
   if (!supported)
      return VDP_STATUS_INVALID_DECODER_PROFILE;

   vldecoder = CALLOC(1,sizeof(vlVdpDecoder));
   if (!vldecoder)
      return VDP_STATUS_RESOURCES;

   vldecoder->device = dev;

   vldecoder->decoder = pipe->create_video_decoder
   (
      pipe, p_profile,
      PIPE_VIDEO_ENTRYPOINT_BITSTREAM,
      PIPE_VIDEO_CHROMA_FORMAT_420,
      width, height, max_references
   );

   if (!vldecoder->decoder) {
      ret = VDP_STATUS_ERROR;
      goto error_decoder;
   }

   *decoder = vlAddDataHTAB(vldecoder);
   if (*decoder == 0) {
      ret = VDP_STATUS_ERROR;
      goto error_handle;
   }

   VDPAU_MSG(VDPAU_TRACE, "[VDPAU] Decoder created succesfully\n");

   return VDP_STATUS_OK;

error_handle:
   vldecoder->decoder->destroy(vldecoder->decoder);

error_decoder:
   FREE(vldecoder);
   return ret;
}

/**
 * Destroy a VdpDecoder.
 */
VdpStatus
vlVdpDecoderDestroy(VdpDecoder decoder)
{
   vlVdpDecoder *vldecoder;

   VDPAU_MSG(VDPAU_TRACE, "[VDPAU] Destroying decoder\n");

   vldecoder = (vlVdpDecoder *)vlGetDataHTAB(decoder);
   if (!vldecoder)
      return VDP_STATUS_INVALID_HANDLE;

   vldecoder->decoder->destroy(vldecoder->decoder);

   FREE(vldecoder);

   return VDP_STATUS_OK;
}

/**
 * Retrieve the parameters used to create a VdpBitmapSurface.
 */
VdpStatus
vlVdpDecoderGetParameters(VdpDecoder decoder,
                          VdpDecoderProfile *profile,
                          uint32_t *width,
                          uint32_t *height)
{
   vlVdpDecoder *vldecoder;

   VDPAU_MSG(VDPAU_TRACE, "[VDPAU] Decoder get parameters called\n");

   vldecoder = (vlVdpDecoder *)vlGetDataHTAB(decoder);
   if (!vldecoder)
      return VDP_STATUS_INVALID_HANDLE;

   *profile = PipeToProfile(vldecoder->decoder->profile);
   *width = vldecoder->decoder->width;
   *height = vldecoder->decoder->height;

   return VDP_STATUS_OK;
}

/**
 * Decode a mpeg 1/2 video.
 */
static VdpStatus
vlVdpDecoderRenderMpeg12(struct pipe_mpeg12_picture_desc *picture,
                         VdpPictureInfoMPEG1Or2 *picture_info)
{
   VDPAU_MSG(VDPAU_TRACE, "[VDPAU] Decoding MPEG12\n");

   /* if surfaces equals VDP_STATUS_INVALID_HANDLE, they are not used */
   if (picture_info->forward_reference !=  VDP_INVALID_HANDLE) {
      picture->ref_forward = ((vlVdpSurface *)vlGetDataHTAB(picture_info->forward_reference))->video_buffer;
      if (!picture->ref_forward)
         return VDP_STATUS_INVALID_HANDLE;
   }
   else
      picture->ref_forward = NULL;

   if (picture_info->backward_reference !=  VDP_INVALID_HANDLE) {
      picture->ref_backward = ((vlVdpSurface *)vlGetDataHTAB(picture_info->backward_reference))->video_buffer;
      if (!picture->ref_backward)
         return VDP_STATUS_INVALID_HANDLE;
   }
   else
      picture->ref_backward = NULL;

   picture->picture_coding_type = picture_info->picture_coding_type;
   picture->picture_structure = picture_info->picture_structure;
   picture->frame_pred_frame_dct = picture_info->frame_pred_frame_dct;
   picture->q_scale_type = picture_info->q_scale_type;
   picture->alternate_scan = picture_info->alternate_scan;
   picture->intra_vlc_format = picture_info->intra_vlc_format;
   picture->concealment_motion_vectors = picture_info->concealment_motion_vectors;
   picture->intra_dc_precision = picture_info->intra_dc_precision;
   picture->f_code[0][0] = picture_info->f_code[0][0] - 1;
   picture->f_code[0][1] = picture_info->f_code[0][1] - 1;
   picture->f_code[1][0] = picture_info->f_code[1][0] - 1;
   picture->f_code[1][1] = picture_info->f_code[1][1] - 1;
   picture->num_slices = picture_info->slice_count;
   picture->top_field_first = picture_info->top_field_first;
   picture->full_pel_forward_vector = picture_info->full_pel_forward_vector;
   picture->full_pel_backward_vector = picture_info->full_pel_backward_vector;
   picture->intra_matrix = picture_info->intra_quantizer_matrix;
   picture->non_intra_matrix = picture_info->non_intra_quantizer_matrix;
   return VDP_STATUS_OK;
}

/**
 * Decode a mpeg 4 video.
 */
static VdpStatus
vlVdpDecoderRenderMpeg4(struct pipe_mpeg4_picture_desc *picture,
                        VdpPictureInfoMPEG4Part2 *picture_info)
{
   unsigned i;

   VDPAU_MSG(VDPAU_TRACE, "[VDPAU] Decoding MPEG4\n");

   if (picture_info->forward_reference != VDP_INVALID_HANDLE) {
      picture->ref_forward = ((vlVdpSurface *)vlGetDataHTAB(picture_info->forward_reference))->video_buffer;
      if (!picture->ref_forward)
         return VDP_STATUS_INVALID_HANDLE;
   }
   else
      picture->ref_forward = NULL;

   if (picture_info->backward_reference != VDP_INVALID_HANDLE) {
      picture->ref_backward = ((vlVdpSurface *)vlGetDataHTAB(picture_info->backward_reference))->video_buffer;
      if (!picture->ref_backward)
         return VDP_STATUS_INVALID_HANDLE;
   }
   else
      picture->ref_backward = NULL;

   for (i = 0; i < 2; ++i) {
      picture->trd[i] = picture_info->trd[i];
      picture->trb[i] = picture_info->trb[i];
   }
   picture->vop_time_increment_resolution = picture_info->vop_time_increment_resolution;
   picture->vop_coding_type = picture_info->vop_coding_type;
   picture->vop_fcode_forward = picture_info->vop_fcode_forward;
   picture->vop_fcode_backward = picture_info->vop_fcode_backward;
   picture->resync_marker_disable = picture_info->resync_marker_disable;
   picture->interlaced = picture_info->interlaced;
   picture->quant_type = picture_info->quant_type;
   picture->quarter_sample = picture_info->quarter_sample;
   picture->short_video_header = picture_info->short_video_header;
   picture->rounding_control = picture_info->rounding_control;
   picture->alternate_vertical_scan_flag = picture_info->alternate_vertical_scan_flag;
   picture->top_field_first = picture_info->top_field_first;
   picture->intra_matrix = picture_info->intra_quantizer_matrix;
   picture->non_intra_matrix = picture_info->non_intra_quantizer_matrix;
   return VDP_STATUS_OK;
}

static VdpStatus
vlVdpDecoderRenderVC1(struct pipe_vc1_picture_desc *picture,
                      VdpPictureInfoVC1 *picture_info)
{
   VDPAU_MSG(VDPAU_TRACE, "[VDPAU] Decoding VC-1\n");

   if (picture_info->forward_reference !=  VDP_INVALID_HANDLE) {
      picture->ref_forward = ((vlVdpSurface *)vlGetDataHTAB(picture_info->forward_reference))->video_buffer;
      if (!picture->ref_forward)
         return VDP_STATUS_INVALID_HANDLE;
   }
   else
      picture->ref_forward = NULL;

   if (picture_info->backward_reference !=  VDP_INVALID_HANDLE) {
      picture->ref_backward = ((vlVdpSurface *)vlGetDataHTAB(picture_info->backward_reference))->video_buffer;
      if (!picture->ref_backward)
         return VDP_STATUS_INVALID_HANDLE;
   }
   else
      picture->ref_backward = NULL;

   picture->slice_count = picture_info->slice_count;
   picture->picture_type = picture_info->picture_type;
   picture->frame_coding_mode = picture_info->frame_coding_mode;
   picture->postprocflag = picture_info->postprocflag;
   picture->pulldown = picture_info->pulldown;
   picture->interlace = picture_info->interlace;
   picture->tfcntrflag = picture_info->tfcntrflag;
   picture->finterpflag = picture_info->finterpflag;
   picture->psf = picture_info->psf;
   picture->dquant = picture_info->dquant;
   picture->panscan_flag = picture_info->panscan_flag;
   picture->refdist_flag = picture_info->refdist_flag;
   picture->quantizer = picture_info->quantizer;
   picture->extended_mv = picture_info->extended_mv;
   picture->extended_dmv = picture_info->extended_dmv;
   picture->overlap = picture_info->overlap;
   picture->vstransform = picture_info->vstransform;
   picture->loopfilter = picture_info->loopfilter;
   picture->fastuvmc = picture_info->fastuvmc;
   picture->range_mapy_flag = picture_info->range_mapy_flag;
   picture->range_mapy = picture_info->range_mapy;
   picture->range_mapuv_flag = picture_info->range_mapuv_flag;
   picture->range_mapuv = picture_info->range_mapuv;
   picture->multires = picture_info->multires;
   picture->syncmarker = picture_info->syncmarker;
   picture->rangered = picture_info->rangered;
   picture->maxbframes = picture_info->maxbframes;
   picture->deblockEnable = picture_info->deblockEnable;
   picture->pquant = picture_info->pquant;
   return VDP_STATUS_OK;
}

static VdpStatus
vlVdpDecoderRenderH264(struct pipe_h264_picture_desc *p,
                       VdpPictureInfoH264 *vdp)
{
   unsigned i;
   VDPAU_MSG(VDPAU_TRACE, "[VDPAU] Decoding H264\n");

   p->slice_count = vdp->slice_count;
   p->field_order_cnt[0] = vdp->field_order_cnt[0];
   p->field_order_cnt[1] = vdp->field_order_cnt[1];
   p->is_reference = vdp->is_reference;

   p->frame_num = vdp->frame_num;
   p->field_pic_flag = vdp->field_pic_flag;
   p->bottom_field_flag = vdp->bottom_field_flag;
   p->num_ref_frames = vdp->num_ref_frames;
   p->mb_adaptive_frame_field_flag = vdp->mb_adaptive_frame_field_flag;
   p->constrained_intra_pred_flag = vdp->constrained_intra_pred_flag;
   p->weighted_pred_flag = vdp->weighted_pred_flag;
   p->weighted_bipred_idc = vdp->weighted_bipred_idc;
   p->frame_mbs_only_flag = vdp->frame_mbs_only_flag;
   p->transform_8x8_mode_flag = vdp->transform_8x8_mode_flag;
   p->chroma_qp_index_offset = vdp->chroma_qp_index_offset;
   p->second_chroma_qp_index_offset = vdp->second_chroma_qp_index_offset;
   p->pic_init_qp_minus26 = vdp->pic_init_qp_minus26;
   p->num_ref_idx_l0_active_minus1 = vdp->num_ref_idx_l0_active_minus1;
   p->num_ref_idx_l1_active_minus1 = vdp->num_ref_idx_l1_active_minus1;
   p->log2_max_frame_num_minus4 = vdp->log2_max_frame_num_minus4;
   p->pic_order_cnt_type = vdp->pic_order_cnt_type;
   p->log2_max_pic_order_cnt_lsb_minus4 = vdp->log2_max_pic_order_cnt_lsb_minus4;
   p->delta_pic_order_always_zero_flag = vdp->delta_pic_order_always_zero_flag;
   p->direct_8x8_inference_flag = vdp->direct_8x8_inference_flag;
   p->entropy_coding_mode_flag = vdp->entropy_coding_mode_flag;
   p->pic_order_present_flag = vdp->pic_order_present_flag;
   p->deblocking_filter_control_present_flag = vdp->deblocking_filter_control_present_flag;
   p->redundant_pic_cnt_present_flag = vdp->redundant_pic_cnt_present_flag;

   p->scaling_lists_4x4 = vdp->scaling_lists_4x4[0];
   p->scaling_lists_8x8 = vdp->scaling_lists_8x8[0];
   for (i = 0; i < 16; ++i) {
      struct pipe_h264_reference_frame *ref = &p->refs[i];
      VdpReferenceFrameH264 *vdp_ref = &vdp->referenceFrames[i];
      if (vdp_ref->surface != VDP_INVALID_HANDLE) {
         vlVdpSurface *surf = vlGetDataHTAB(vdp_ref->surface);
         if (!surf)
            return VDP_STATUS_INVALID_HANDLE;
         ref->surface = surf->video_buffer;
      } else
         ref->surface = NULL;
      ref->is_long_term = vdp_ref->is_long_term;
      ref->top_is_reference = vdp_ref->top_is_reference;
      ref->bottom_is_reference = vdp_ref->bottom_is_reference;
      ref->field_order_cnt[0] = vdp_ref->field_order_cnt[0];
      ref->field_order_cnt[1] = vdp_ref->field_order_cnt[1];
      ref->frame_idx = vdp_ref->frame_idx;
   }
   return VDP_STATUS_OK;
}

/**
 * Decode a compressed field/frame and render the result into a VdpVideoSurface.
 */
VdpStatus
vlVdpDecoderRender(VdpDecoder decoder,
                   VdpVideoSurface target,
                   VdpPictureInfo const *picture_info,
                   uint32_t bitstream_buffer_count,
                   VdpBitstreamBuffer const *bitstream_buffers)
{
   vlVdpDecoder *vldecoder;
   vlVdpSurface *vlsurf;
   VdpStatus ret;
   struct pipe_video_decoder *dec;
   unsigned i;
   union {
      struct pipe_picture_desc base;
      struct pipe_mpeg12_picture_desc mpeg12;
      struct pipe_mpeg4_picture_desc mpeg4;
      struct pipe_vc1_picture_desc vc1;
      struct pipe_h264_picture_desc h264;
   } desc;

   VDPAU_MSG(VDPAU_TRACE, "[VDPAU] Decoding\n");

   if (!(picture_info && bitstream_buffers))
      return VDP_STATUS_INVALID_POINTER;

   vldecoder = (vlVdpDecoder *)vlGetDataHTAB(decoder);
   if (!vldecoder)
      return VDP_STATUS_INVALID_HANDLE;
   dec = vldecoder->decoder;

   vlsurf = (vlVdpSurface *)vlGetDataHTAB(target);
   if (!vlsurf)
      return VDP_STATUS_INVALID_HANDLE;

   if (vlsurf->device != vldecoder->device)
      return VDP_STATUS_HANDLE_DEVICE_MISMATCH;

   if (vlsurf->video_buffer->chroma_format != dec->chroma_format)
      // TODO: Recreate decoder with correct chroma
      return VDP_STATUS_INVALID_CHROMA_TYPE;

   memset(&desc, 0, sizeof(desc));
   desc.base.profile = dec->profile;
   switch (u_reduce_video_profile(dec->profile)) {
   case PIPE_VIDEO_CODEC_MPEG12:
      ret = vlVdpDecoderRenderMpeg12(&desc.mpeg12, (VdpPictureInfoMPEG1Or2 *)picture_info);
      break;
   case PIPE_VIDEO_CODEC_MPEG4:
      ret = vlVdpDecoderRenderMpeg4(&desc.mpeg4, (VdpPictureInfoMPEG4Part2 *)picture_info);
      break;
   case PIPE_VIDEO_CODEC_VC1:
      ret = vlVdpDecoderRenderVC1(&desc.vc1, (VdpPictureInfoVC1 *)picture_info);
      break;
   case PIPE_VIDEO_CODEC_MPEG4_AVC:
      ret = vlVdpDecoderRenderH264(&desc.h264, (VdpPictureInfoH264 *)picture_info);
      break;
   default:
      return VDP_STATUS_INVALID_DECODER_PROFILE;
   }
   if (ret != VDP_STATUS_OK)
      return ret;

   if (bitstream_buffer_count) {
      void const *data[bitstream_buffer_count];
      uint32_t sizes[bitstream_buffer_count];
      uint32_t total_size = 0;
      for (i = 0; i < bitstream_buffer_count; ++i) {
         data[i] = bitstream_buffers[i].bitstream;
         sizes[i] = bitstream_buffers[i].bitstream_bytes;
         total_size += sizes[i];
      }
      dec->decode_bitstream(dec, vlsurf->video_buffer, &desc.base,
                            bitstream_buffer_count, total_size,
                            sizes, data);
   } else
      return VDP_STATUS_INVALID_VALUE;
   return ret;
}
