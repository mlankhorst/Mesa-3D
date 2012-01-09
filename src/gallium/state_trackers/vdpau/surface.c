/**************************************************************************
 *
 * Copyright 2010 Thomas Balling Sørensen.
 * Copyright 2011 Christian König.
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

#include <assert.h>

#include "pipe/p_state.h"

#include "util/u_memory.h"
#include "util/u_debug.h"
#include "util/u_rect.h"

#include "vdpau_private.h"

/**
 * Create a VdpVideoSurface.
 */
VdpStatus
vlVdpVideoSurfaceCreate(VdpDevice device, VdpChromaType chroma_type,
                        uint32_t width, uint32_t height,
                        VdpVideoSurface *surface)
{
   vlVdpSurface *p_surf;
   VdpStatus ret;

   VDPAU_MSG(VDPAU_TRACE, "[VDPAU] Creating a surface\n");

   if (!(width && height)) {
      ret = VDP_STATUS_INVALID_SIZE;
      goto inv_size;
   }

   if (!vlCreateHTAB()) {
      ret = VDP_STATUS_RESOURCES;
      goto no_htab;
   }

   p_surf = CALLOC(1, sizeof(vlVdpSurface));
   if (!p_surf) {
      ret = VDP_STATUS_RESOURCES;
      goto no_res;
   }

   vlVdpDevice *dev = vlGetDataHTAB(device);
   if (!dev) {
      ret = VDP_STATUS_INVALID_HANDLE;
      goto inv_device;
   }

   p_surf->device = dev;
   p_surf->video_buffer = dev->context->pipe->create_video_buffer
   (
      dev->context->pipe,
      PIPE_FORMAT_YV12, // most common used
      ChromaToPipe(chroma_type),
      width, height
   );

   *surface = vlAddDataHTAB(p_surf);
   if (*surface == 0) {
      ret = VDP_STATUS_ERROR;
      goto no_handle;
   }

   return VDP_STATUS_OK;

no_handle:
   p_surf->video_buffer->destroy(p_surf->video_buffer);

inv_device:
   FREE(p_surf);

no_res:
no_htab:
inv_size:
   return ret;
}

/**
 * Destroy a VdpVideoSurface.
 */
VdpStatus
vlVdpVideoSurfaceDestroy(VdpVideoSurface surface)
{
   vlVdpSurface *p_surf;

   p_surf = (vlVdpSurface *)vlGetDataHTAB((vlHandle)surface);
   if (!p_surf)
      return VDP_STATUS_INVALID_HANDLE;

   if (p_surf->video_buffer)
      p_surf->video_buffer->destroy(p_surf->video_buffer);

   FREE(p_surf);
   return VDP_STATUS_OK;
}

/**
 * Retrieve the parameters used to create a VdpVideoSurface.
 */
VdpStatus
vlVdpVideoSurfaceGetParameters(VdpVideoSurface surface,
                               VdpChromaType *chroma_type,
                               uint32_t *width, uint32_t *height)
{
   if (!(width && height && chroma_type))
      return VDP_STATUS_INVALID_POINTER;

   vlVdpSurface *p_surf = vlGetDataHTAB(surface);
   if (!p_surf)
      return VDP_STATUS_INVALID_HANDLE;

   *width = p_surf->video_buffer->width;
   *height = p_surf->video_buffer->height;
   *chroma_type = PipeToChroma(p_surf->video_buffer->chroma_format);

   return VDP_STATUS_OK;
}

/**
 * Copy image data from a VdpVideoSurface to application memory in a specified
 * YCbCr format.
 */
VdpStatus
vlVdpVideoSurfaceGetBitsYCbCr(VdpVideoSurface surface,
                              VdpYCbCrFormat destination_ycbcr_format,
                              void *const *destination_data,
                              uint32_t const *destination_pitches)
{
   if (!vlCreateHTAB())
      return VDP_STATUS_RESOURCES;

   vlVdpSurface *p_surf = vlGetDataHTAB(surface);
   if (!p_surf)
      return VDP_STATUS_INVALID_HANDLE;

   //if (!p_surf->psurface)
   //   return VDP_STATUS_RESOURCES;

   //return VDP_STATUS_OK;
   return VDP_STATUS_NO_IMPLEMENTATION;
}

/* Really, don't try to copy between everything here..
 */
static void
vl_vdp_copy(enum pipe_format dst_format,
            enum pipe_format src_format,
            enum pipe_format sv_format,
            unsigned char *map,
            unsigned interlaced,
            unsigned i,
            void const *const *addrs,
            uint32_t const *pitches,
            unsigned transfer_stride,
            unsigned w,
            unsigned h)
{
   unsigned j = i>>interlaced;
   if (src_format == dst_format)
      goto match;
   if ((dst_format == PIPE_FORMAT_NV12 || dst_format == PIPE_FORMAT_YV12) &&
       (src_format == PIPE_FORMAT_NV12 || src_format == PIPE_FORMAT_YV12)) {
      if (!j) {
         goto match;
      } else if (dst_format == PIPE_FORMAT_NV12) {
         /* YV12 -> NV12 cbcr copy */
         for (j = 1; j < 3; ++j) {
            unsigned pitch = pitches[j] << interlaced;
            const unsigned char *src = addrs[j];
            char unsigned *dst = map + (j == 1);
            unsigned lines, k;
            if (i & interlaced)
               src += pitches[j];
            for (lines = h; lines; --lines) {
               for (k = 0; k < w; ++k)
                  dst[k*2] = src[k];
               src += pitch;
               dst += transfer_stride;
            }
            assert(w*2 <= transfer_stride);
            assert(w <= pitches[j]);
         }
         return;
      }
   }
   assert(0);
   return;

match:
   {
      unsigned pitch = pitches[j] << interlaced;
      const unsigned char *addr = addrs[j];
      if (i & interlaced)
         addr += pitches[j];
      util_copy_rect(map, sv_format, transfer_stride, 0, 0,
                     w, h, addr, pitch, 0, 0);
      }
}

static VdpStatus
vl_vdp_upload(struct pipe_video_buffer *buf,
              enum pipe_format format,
              struct pipe_sampler_view **sampler_views,
              unsigned interlaced,
              void const *const *addrs,
              uint32_t const *pitches)
{
   unsigned p = interlaced ? 6 : 3;
   unsigned i, j, h, w;
   struct pipe_context *pipe = buf->context;

   for (i = 0; i < p; ++i) {
      struct pipe_transfer *transfer;
      void *map;
      h = buf->height;
      j = i>>interlaced;
      if (interlaced)
         h = (h + !(i&1))/2;
      if (j && buf->chroma_format != PIPE_VIDEO_CHROMA_FORMAT_444)
         h = (h + 1)/2;
      w = buf->width;
      if (j && buf->chroma_format == PIPE_VIDEO_CHROMA_FORMAT_420)
         w = (w + 1)/2;

      struct pipe_sampler_view *sv;
      if (j && format == PIPE_FORMAT_YV12 && buf->buffer_format == format)
         sv = sampler_views[i^p];
      else
         sv = sampler_views[i];
      if (!sv)
         return VDP_STATUS_OK;
      struct pipe_box dst_box = { 0, 0, sv->u.tex.first_layer, w, h, 1 };

      transfer = pipe->get_transfer(pipe, sv->texture, 0, PIPE_TRANSFER_WRITE, &dst_box);
      if (!transfer)
         return VDP_STATUS_RESOURCES;

      map = pipe->transfer_map(pipe, transfer);
      if (map) {
         vl_vdp_copy(buf->buffer_format, format, sv->texture->format, map, interlaced, i, addrs, pitches, transfer->stride, w, h);
         pipe->transfer_unmap(pipe, transfer);
      }

      pipe->transfer_destroy(pipe, transfer);
   }
   return VDP_STATUS_OK;
}

/**
 * Copy image data from application memory in a specific YCbCr format to
 * a VdpVideoSurface.
 */
VdpStatus
vlVdpVideoSurfacePutBitsYCbCr(VdpVideoSurface surface,
                              VdpYCbCrFormat source_ycbcr_format,
                              void const *const *source_data,
                              uint32_t const *source_pitches)
{
   enum pipe_format pformat = FormatYCBCRToPipe(source_ycbcr_format);
   struct pipe_context *pipe;
   struct pipe_sampler_view **sampler_views;

   if (!vlCreateHTAB())
      return VDP_STATUS_RESOURCES;

   vlVdpSurface *p_surf = vlGetDataHTAB(surface);
   if (!p_surf)
      return VDP_STATUS_INVALID_HANDLE;

   pipe = p_surf->device->context->pipe;
   if (!pipe)
      return VDP_STATUS_INVALID_HANDLE;

   sampler_views = p_surf->video_buffer->get_sampler_view_planes(p_surf->video_buffer, 0);
   if (sampler_views)
      return vl_vdp_upload(p_surf->video_buffer, pformat, sampler_views, 0, source_data, source_pitches);
   sampler_views = p_surf->video_buffer->get_sampler_view_planes(p_surf->video_buffer, 1);
   assert(sampler_views);
   return vl_vdp_upload(p_surf->video_buffer, pformat, sampler_views, 1, source_data, source_pitches);
}
