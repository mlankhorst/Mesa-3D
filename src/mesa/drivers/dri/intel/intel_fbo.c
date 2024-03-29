/**************************************************************************
 * 
 * Copyright 2006 Tungsten Graphics, Inc., Cedar Park, Texas.
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


#include "main/enums.h"
#include "main/imports.h"
#include "main/macros.h"
#include "main/mfeatures.h"
#include "main/mtypes.h"
#include "main/fbobject.h"
#include "main/framebuffer.h"
#include "main/renderbuffer.h"
#include "main/context.h"
#include "main/teximage.h"
#include "main/image.h"

#include "swrast/swrast.h"
#include "drivers/common/meta.h"

#include "intel_context.h"
#include "intel_batchbuffer.h"
#include "intel_buffers.h"
#include "intel_blit.h"
#include "intel_fbo.h"
#include "intel_mipmap_tree.h"
#include "intel_regions.h"
#include "intel_tex.h"
#include "intel_span.h"
#ifndef I915
#include "brw_context.h"
#endif

#define FILE_DEBUG_FLAG DEBUG_FBO

static struct gl_renderbuffer *
intel_new_renderbuffer(struct gl_context * ctx, GLuint name);

static bool
intel_renderbuffer_update_wrapper(struct intel_context *intel,
                                  struct intel_renderbuffer *irb,
                                  struct intel_mipmap_tree *mt,
                                  uint32_t level,
                                  uint32_t layer,
                                  gl_format format,
                                  GLenum internal_format);

bool
intel_framebuffer_has_hiz(struct gl_framebuffer *fb)
{
   struct intel_renderbuffer *rb = NULL;
   if (fb)
      rb = intel_get_renderbuffer(fb, BUFFER_DEPTH);
   return rb && rb->mt && rb->mt->hiz_mt;
}

struct intel_region*
intel_get_rb_region(struct gl_framebuffer *fb, GLuint attIndex)
{
   struct intel_renderbuffer *irb = intel_get_renderbuffer(fb, attIndex);
   if (irb && irb->mt) {
      if (attIndex == BUFFER_STENCIL && irb->mt->stencil_mt)
	 return irb->mt->stencil_mt->region;
      else
	 return irb->mt->region;
   } else
      return NULL;
}

/**
 * Create a new framebuffer object.
 */
static struct gl_framebuffer *
intel_new_framebuffer(struct gl_context * ctx, GLuint name)
{
   /* Only drawable state in intel_framebuffer at this time, just use Mesa's
    * class
    */
   return _mesa_new_framebuffer(ctx, name);
}


/** Called by gl_renderbuffer::Delete() */
static void
intel_delete_renderbuffer(struct gl_renderbuffer *rb)
{
   struct intel_renderbuffer *irb = intel_renderbuffer(rb);

   ASSERT(irb);

   intel_miptree_release(&irb->mt);

   free(irb);
}

/**
 * \see dd_function_table::MapRenderbuffer
 */
static void
intel_map_renderbuffer(struct gl_context *ctx,
		       struct gl_renderbuffer *rb,
		       GLuint x, GLuint y, GLuint w, GLuint h,
		       GLbitfield mode,
		       GLubyte **out_map,
		       GLint *out_stride)
{
   struct intel_context *intel = intel_context(ctx);
   struct intel_renderbuffer *irb = intel_renderbuffer(rb);
   void *map;
   int stride;

   /* We sometimes get called with this by our intel_span.c usage. */
   if (!irb->mt) {
      *out_map = NULL;
      *out_stride = 0;
      return;
   }

   /* For a window-system renderbuffer, we need to flip the mapping we receive
    * upside-down.  So we need to ask for a rectangle on flipped vertically, and
    * we then return a pointer to the bottom of it with a negative stride.
    */
   if (rb->Name == 0) {
      y = rb->Height - y - h;
   }

   intel_miptree_map(intel, irb->mt, irb->mt_level, irb->mt_layer,
		     x, y, w, h, mode, &map, &stride);

   if (rb->Name == 0) {
      map += (h - 1) * stride;
      stride = -stride;
   }

   DBG("%s: rb %d (%s) mt mapped: (%d, %d) (%dx%d) -> %p/%d\n",
       __FUNCTION__, rb->Name, _mesa_get_format_name(rb->Format),
       x, y, w, h, map, stride);

   *out_map = map;
   *out_stride = stride;
}

/**
 * \see dd_function_table::UnmapRenderbuffer
 */
static void
intel_unmap_renderbuffer(struct gl_context *ctx,
			 struct gl_renderbuffer *rb)
{
   struct intel_context *intel = intel_context(ctx);
   struct intel_renderbuffer *irb = intel_renderbuffer(rb);

   DBG("%s: rb %d (%s)\n", __FUNCTION__,
       rb->Name, _mesa_get_format_name(rb->Format));

   intel_miptree_unmap(intel, irb->mt, irb->mt_level, irb->mt_layer);
}

/**
 * Return a pointer to a specific pixel in a renderbuffer.
 */
static void *
intel_get_pointer(struct gl_context * ctx, struct gl_renderbuffer *rb,
                  GLint x, GLint y)
{
   /* By returning NULL we force all software rendering to go through
    * the span routines.
    */
   return NULL;
}


/**
 * Called via glRenderbufferStorageEXT() to set the format and allocate
 * storage for a user-created renderbuffer.
 */
GLboolean
intel_alloc_renderbuffer_storage(struct gl_context * ctx, struct gl_renderbuffer *rb,
                                 GLenum internalFormat,
                                 GLuint width, GLuint height)
{
   struct intel_context *intel = intel_context(ctx);
   struct intel_renderbuffer *irb = intel_renderbuffer(rb);

   ASSERT(rb->Name != 0);

   switch (internalFormat) {
   default:
      /* Use the same format-choice logic as for textures.
       * Renderbuffers aren't any different from textures for us,
       * except they're less useful because you can't texture with
       * them.
       */
      rb->Format = intel->ctx.Driver.ChooseTextureFormat(ctx, internalFormat,
							 GL_NONE, GL_NONE);
      break;
   case GL_STENCIL_INDEX:
   case GL_STENCIL_INDEX1_EXT:
   case GL_STENCIL_INDEX4_EXT:
   case GL_STENCIL_INDEX8_EXT:
   case GL_STENCIL_INDEX16_EXT:
      /* These aren't actual texture formats, so force them here. */
      if (intel->has_separate_stencil) {
	 rb->Format = MESA_FORMAT_S8;
      } else {
	 assert(!intel->must_use_separate_stencil);
	 rb->Format = MESA_FORMAT_S8_Z24;
      }
      break;
   }

   rb->Width = width;
   rb->Height = height;
   rb->_BaseFormat = _mesa_base_fbo_format(ctx, internalFormat);
   rb->DataType = intel_mesa_format_to_rb_datatype(rb->Format);

   intel_flush(ctx);

   intel_miptree_release(&irb->mt);

   DBG("%s: %s: %s (%dx%d)\n", __FUNCTION__,
       _mesa_lookup_enum_by_nr(internalFormat),
       _mesa_get_format_name(rb->Format), width, height);

   irb->mt = intel_miptree_create_for_renderbuffer(intel, rb->Format,
						   width, height);
   if (!irb->mt)
      return false;

   if (intel->vtbl.is_hiz_depth_format(intel, rb->Format)) {
      bool ok = intel_miptree_alloc_hiz(intel, irb->mt);
      if (!ok) {
	 intel_miptree_release(&irb->mt);
	 return false;
      }
   }

   return true;
}


#if FEATURE_OES_EGL_image
static void
intel_image_target_renderbuffer_storage(struct gl_context *ctx,
					struct gl_renderbuffer *rb,
					void *image_handle)
{
   struct intel_context *intel = intel_context(ctx);
   struct intel_renderbuffer *irb;
   __DRIscreen *screen;
   __DRIimage *image;

   screen = intel->intelScreen->driScrnPriv;
   image = screen->dri2.image->lookupEGLImage(screen, image_handle,
					      screen->loaderPrivate);
   if (image == NULL)
      return;

   /* __DRIimage is opaque to the core so it has to be checked here */
   switch (image->format) {
   case MESA_FORMAT_RGBA8888_REV:
      _mesa_error(&intel->ctx, GL_INVALID_OPERATION,
            "glEGLImageTargetRenderbufferStorage(unsupported image format");
      return;
      break;
   default:
      break;
   }

   irb = intel_renderbuffer(rb);
   intel_miptree_release(&irb->mt);
   irb->mt = intel_miptree_create_for_region(intel,
                                             GL_TEXTURE_2D,
                                             image->format,
                                             image->region);
   if (!irb->mt)
      return;

   rb->InternalFormat = image->internal_format;
   rb->Width = image->region->width;
   rb->Height = image->region->height;
   rb->Format = image->format;
   rb->DataType = image->data_type;
   rb->_BaseFormat = _mesa_base_fbo_format(&intel->ctx,
					   image->internal_format);
}
#endif

/**
 * Called for each hardware renderbuffer when a _window_ is resized.
 * Just update fields.
 * Not used for user-created renderbuffers!
 */
static GLboolean
intel_alloc_window_storage(struct gl_context * ctx, struct gl_renderbuffer *rb,
                           GLenum internalFormat, GLuint width, GLuint height)
{
   ASSERT(rb->Name == 0);
   rb->Width = width;
   rb->Height = height;
   rb->InternalFormat = internalFormat;

   return true;
}


static void
intel_resize_buffers(struct gl_context *ctx, struct gl_framebuffer *fb,
		     GLuint width, GLuint height)
{
   int i;

   _mesa_resize_framebuffer(ctx, fb, width, height);

   fb->Initialized = true; /* XXX remove someday */

   if (fb->Name != 0) {
      return;
   }


   /* Make sure all window system renderbuffers are up to date */
   for (i = BUFFER_FRONT_LEFT; i <= BUFFER_BACK_RIGHT; i++) {
      struct gl_renderbuffer *rb = fb->Attachment[i].Renderbuffer;

      /* only resize if size is changing */
      if (rb && (rb->Width != width || rb->Height != height)) {
	 rb->AllocStorage(ctx, rb, rb->InternalFormat, width, height);
      }
   }
}


/** Dummy function for gl_renderbuffer::AllocStorage() */
static GLboolean
intel_nop_alloc_storage(struct gl_context * ctx, struct gl_renderbuffer *rb,
                        GLenum internalFormat, GLuint width, GLuint height)
{
   _mesa_problem(ctx, "intel_op_alloc_storage should never be called.");
   return false;
}

/**
 * Create a new intel_renderbuffer which corresponds to an on-screen window,
 * not a user-created renderbuffer.
 */
struct intel_renderbuffer *
intel_create_renderbuffer(gl_format format)
{
   GET_CURRENT_CONTEXT(ctx);

   struct intel_renderbuffer *irb;

   irb = CALLOC_STRUCT(intel_renderbuffer);
   if (!irb) {
      _mesa_error(ctx, GL_OUT_OF_MEMORY, "creating renderbuffer");
      return NULL;
   }

   _mesa_init_renderbuffer(&irb->Base, 0);
   irb->Base.ClassID = INTEL_RB_CLASS;
   irb->Base._BaseFormat = _mesa_get_format_base_format(format);
   irb->Base.Format = format;
   irb->Base.InternalFormat = irb->Base._BaseFormat;
   irb->Base.DataType = intel_mesa_format_to_rb_datatype(format);

   /* intel-specific methods */
   irb->Base.Delete = intel_delete_renderbuffer;
   irb->Base.AllocStorage = intel_alloc_window_storage;
   irb->Base.GetPointer = intel_get_pointer;

   return irb;
}

/**
 * Create a new renderbuffer object.
 * Typically called via glBindRenderbufferEXT().
 */
static struct gl_renderbuffer *
intel_new_renderbuffer(struct gl_context * ctx, GLuint name)
{
   /*struct intel_context *intel = intel_context(ctx); */
   struct intel_renderbuffer *irb;

   irb = CALLOC_STRUCT(intel_renderbuffer);
   if (!irb) {
      _mesa_error(ctx, GL_OUT_OF_MEMORY, "creating renderbuffer");
      return NULL;
   }

   _mesa_init_renderbuffer(&irb->Base, name);
   irb->Base.ClassID = INTEL_RB_CLASS;

   /* intel-specific methods */
   irb->Base.Delete = intel_delete_renderbuffer;
   irb->Base.AllocStorage = intel_alloc_renderbuffer_storage;
   irb->Base.GetPointer = intel_get_pointer;
   /* span routines set in alloc_storage function */

   return &irb->Base;
}


/**
 * Called via glBindFramebufferEXT().
 */
static void
intel_bind_framebuffer(struct gl_context * ctx, GLenum target,
                       struct gl_framebuffer *fb, struct gl_framebuffer *fbread)
{
   if (target == GL_FRAMEBUFFER_EXT || target == GL_DRAW_FRAMEBUFFER_EXT) {
      intel_draw_buffer(ctx);
   }
   else {
      /* don't need to do anything if target == GL_READ_FRAMEBUFFER_EXT */
   }
}


/**
 * Called via glFramebufferRenderbufferEXT().
 */
static void
intel_framebuffer_renderbuffer(struct gl_context * ctx,
                               struct gl_framebuffer *fb,
                               GLenum attachment, struct gl_renderbuffer *rb)
{
   DBG("Intel FramebufferRenderbuffer %u %u\n", fb->Name, rb ? rb->Name : 0);

   intel_flush(ctx);

   _mesa_framebuffer_renderbuffer(ctx, fb, attachment, rb);
   intel_draw_buffer(ctx);
}

static struct intel_renderbuffer*
intel_renderbuffer_wrap_miptree(struct intel_context *intel,
                                struct intel_mipmap_tree *mt,
                                uint32_t level,
                                uint32_t layer,
                                gl_format format,
                                GLenum internal_format);

/**
 * \par Special case for separate stencil
 *
 *     When wrapping a depthstencil texture that uses separate stencil, this
 *     function is recursively called twice: once to create \c
 *     irb->wrapped_depth and again to create \c irb->wrapped_stencil.  On the
 *     call to create \c irb->wrapped_depth, the \c format and \c
 *     internal_format parameters do not match \c mt->format. In that case, \c
 *     mt->format is MESA_FORMAT_S8_Z24 and \c format is \c
 *     MESA_FORMAT_X8_Z24.
 *
 * @return true on success
 */
static bool
intel_renderbuffer_update_wrapper(struct intel_context *intel,
                                  struct intel_renderbuffer *irb,
                                  struct intel_mipmap_tree *mt,
                                  uint32_t level,
                                  uint32_t layer,
                                  gl_format format,
                                  GLenum internal_format)
{
   struct gl_renderbuffer *rb = &irb->Base;

   rb->Format = format;
   rb->InternalFormat = internal_format;
   rb->DataType = intel_mesa_format_to_rb_datatype(rb->Format);
   rb->_BaseFormat = _mesa_get_format_base_format(rb->Format);
   rb->Width = mt->level[level].width;
   rb->Height = mt->level[level].height;

   irb->Base.Delete = intel_delete_renderbuffer;
   irb->Base.AllocStorage = intel_nop_alloc_storage;

   intel_miptree_check_level_layer(mt, level, layer);
   irb->mt_level = level;
   irb->mt_layer = layer;

   intel_miptree_reference(&irb->mt, mt);

   intel_renderbuffer_set_draw_offset(irb);

   if (mt->hiz_mt == NULL &&
       intel->vtbl.is_hiz_depth_format(intel, rb->Format)) {
      intel_miptree_alloc_hiz(intel, mt);
      if (!mt->hiz_mt)
	 return false;
   }

   return true;
}

/**
 * \brief Wrap a renderbuffer around a single slice of a miptree.
 *
 * Called by glFramebufferTexture*(). This just allocates a
 * ``struct intel_renderbuffer`` then calls
 * intel_renderbuffer_update_wrapper() to do the real work.
 *
 * \see intel_renderbuffer_update_wrapper()
 */
static struct intel_renderbuffer*
intel_renderbuffer_wrap_miptree(struct intel_context *intel,
                                struct intel_mipmap_tree *mt,
                                uint32_t level,
                                uint32_t layer,
                                gl_format format,
                                GLenum internal_format)

{
   struct gl_context *ctx = &intel->ctx;
   struct gl_renderbuffer *rb;
   struct intel_renderbuffer *irb;

   intel_miptree_check_level_layer(mt, level, layer);

   rb = intel_new_renderbuffer(ctx, ~0);
   irb = intel_renderbuffer(rb);
   if (!irb)
      return NULL;

   if (!intel_renderbuffer_update_wrapper(intel, irb,
                                          mt, level, layer,
                                          format, internal_format)) {
      free(irb);
      return NULL;
   }

   return irb;
}

void
intel_renderbuffer_set_draw_offset(struct intel_renderbuffer *irb)
{
   unsigned int dst_x, dst_y;

   /* compute offset of the particular 2D image within the texture region */
   intel_miptree_get_image_offset(irb->mt,
				  irb->mt_level,
				  0, /* face, which we ignore */
				  irb->mt_layer,
				  &dst_x, &dst_y);

   irb->draw_x = dst_x;
   irb->draw_y = dst_y;
}

/**
 * Rendering to tiled buffers requires that the base address of the
 * buffer be aligned to a page boundary.  We generally render to
 * textures by pointing the surface at the mipmap image level, which
 * may not be aligned to a tile boundary.
 *
 * This function returns an appropriately-aligned base offset
 * according to the tiling restrictions, plus any required x/y offset
 * from there.
 */
uint32_t
intel_renderbuffer_tile_offsets(struct intel_renderbuffer *irb,
				uint32_t *tile_x,
				uint32_t *tile_y)
{
   struct intel_region *region = irb->mt->region;
   int cpp = region->cpp;
   uint32_t pitch = region->pitch * cpp;

   if (region->tiling == I915_TILING_NONE) {
      *tile_x = 0;
      *tile_y = 0;
      return irb->draw_x * cpp + irb->draw_y * pitch;
   } else if (region->tiling == I915_TILING_X) {
      *tile_x = irb->draw_x % (512 / cpp);
      *tile_y = irb->draw_y % 8;
      return ((irb->draw_y / 8) * (8 * pitch) +
	      (irb->draw_x - *tile_x) / (512 / cpp) * 4096);
   } else {
      assert(region->tiling == I915_TILING_Y);
      *tile_x = irb->draw_x % (128 / cpp);
      *tile_y = irb->draw_y % 32;
      return ((irb->draw_y / 32) * (32 * pitch) +
	      (irb->draw_x - *tile_x) / (128 / cpp) * 4096);
   }
}

#ifndef I915
static bool
need_tile_offset_workaround(struct brw_context *brw,
			    struct intel_renderbuffer *irb)
{
   uint32_t tile_x, tile_y;

   if (brw->has_surface_tile_offset)
      return false;

   intel_renderbuffer_tile_offsets(irb, &tile_x, &tile_y);

   return tile_x != 0 || tile_y != 0;
}
#endif

/**
 * Called by glFramebufferTexture[123]DEXT() (and other places) to
 * prepare for rendering into texture memory.  This might be called
 * many times to choose different texture levels, cube faces, etc
 * before intel_finish_render_texture() is ever called.
 */
static void
intel_render_texture(struct gl_context * ctx,
                     struct gl_framebuffer *fb,
                     struct gl_renderbuffer_attachment *att)
{
   struct intel_context *intel = intel_context(ctx);
   struct gl_texture_image *image = _mesa_get_attachment_teximage(att);
   struct intel_renderbuffer *irb = intel_renderbuffer(att->Renderbuffer);
   struct intel_texture_image *intel_image = intel_texture_image(image);
   struct intel_mipmap_tree *mt = intel_image->mt;

   (void) fb;

   int layer;
   if (att->CubeMapFace > 0) {
      assert(att->Zoffset == 0);
      layer = att->CubeMapFace;
   } else {
      layer = att->Zoffset;
   }

   if (!intel_image->mt) {
      /* Fallback on drawing to a texture that doesn't have a miptree
       * (has a border, width/height 0, etc.)
       */
      _mesa_reference_renderbuffer(&att->Renderbuffer, NULL);
      _swrast_render_texture(ctx, fb, att);
      return;
   }
   else if (!irb) {
      irb = intel_renderbuffer_wrap_miptree(intel,
                                            mt,
                                            att->TextureLevel,
                                            layer,
                                            image->TexFormat,
                                            image->InternalFormat);

      if (irb) {
         /* bind the wrapper to the attachment point */
         _mesa_reference_renderbuffer(&att->Renderbuffer, &irb->Base);
      }
      else {
         /* fallback to software rendering */
         _swrast_render_texture(ctx, fb, att);
         return;
      }
   }

   if (!intel_renderbuffer_update_wrapper(intel, irb,
                                          mt, att->TextureLevel, layer,
                                          image->TexFormat,
                                          image->InternalFormat)) {
       _mesa_reference_renderbuffer(&att->Renderbuffer, NULL);
       _swrast_render_texture(ctx, fb, att);
       return;
   }

   DBG("Begin render %s texture tex=%u w=%d h=%d refcount=%d\n",
       _mesa_get_format_name(image->TexFormat),
       att->Texture->Name, image->Width, image->Height,
       irb->Base.RefCount);

   intel_image->used_as_render_target = true;

#ifndef I915
   if (need_tile_offset_workaround(brw_context(ctx), irb)) {
      /* Original gen4 hardware couldn't draw to a non-tile-aligned
       * destination in a miptree unless you actually setup your
       * renderbuffer as a miptree and used the fragile
       * lod/array_index/etc. controls to select the image.  So,
       * instead, we just make a new single-level miptree and render
       * into that.
       */
      struct intel_context *intel = intel_context(ctx);
      struct intel_mipmap_tree *new_mt;
      int width, height, depth;

      intel_miptree_get_dimensions_for_image(image, &width, &height, &depth);

      new_mt = intel_miptree_create(intel, image->TexObject->Target,
				    intel_image->base.Base.TexFormat,
				    intel_image->base.Base.Level,
				    intel_image->base.Base.Level,
                                    width, height, depth,
				    true);

      intel_miptree_copy_teximage(intel, intel_image, new_mt);
      intel_renderbuffer_set_draw_offset(irb);

      intel_miptree_reference(&irb->mt, intel_image->mt);
      intel_miptree_release(&new_mt);
   }
#endif
   /* update drawing region, etc */
   intel_draw_buffer(ctx);
}


/**
 * Called by Mesa when rendering to a texture is done.
 */
static void
intel_finish_render_texture(struct gl_context * ctx,
                            struct gl_renderbuffer_attachment *att)
{
   struct intel_context *intel = intel_context(ctx);
   struct gl_texture_object *tex_obj = att->Texture;
   struct gl_texture_image *image =
      tex_obj->Image[att->CubeMapFace][att->TextureLevel];
   struct intel_texture_image *intel_image = intel_texture_image(image);

   DBG("Finish render %s texture tex=%u\n",
       _mesa_get_format_name(image->TexFormat), att->Texture->Name);

   /* Flag that this image may now be validated into the object's miptree. */
   if (intel_image)
      intel_image->used_as_render_target = false;

   /* Since we've (probably) rendered to the texture and will (likely) use
    * it in the texture domain later on in this batchbuffer, flush the
    * batch.  Once again, we wish for a domain tracker in libdrm to cover
    * usage inside of a batchbuffer like GEM does in the kernel.
    */
   intel_batchbuffer_emit_mi_flush(intel);
}

/**
 * Do additional "completeness" testing of a framebuffer object.
 */
static void
intel_validate_framebuffer(struct gl_context *ctx, struct gl_framebuffer *fb)
{
   struct intel_context *intel = intel_context(ctx);
   const struct intel_renderbuffer *depthRb =
      intel_get_renderbuffer(fb, BUFFER_DEPTH);
   const struct intel_renderbuffer *stencilRb =
      intel_get_renderbuffer(fb, BUFFER_STENCIL);
   struct intel_mipmap_tree *depth_mt = NULL, *stencil_mt = NULL;
   int i;

   if (depthRb)
      depth_mt = depthRb->mt;
   if (stencilRb) {
      stencil_mt = stencilRb->mt;
      if (stencil_mt->stencil_mt)
	 stencil_mt = stencil_mt->stencil_mt;
   }

   if (depth_mt && stencil_mt) {
      if (depth_mt == stencil_mt) {
	 /* For true packed depth/stencil (not faked on prefers-separate-stencil
	  * hardware) we need to be sure they're the same level/layer, since
	  * we'll be emitting a single packet describing the packed setup.
	  */
	 if (depthRb->mt_level != stencilRb->mt_level ||
	     depthRb->mt_layer != stencilRb->mt_layer) {
	    fb->_Status = GL_FRAMEBUFFER_UNSUPPORTED_EXT;
	 }
      } else {
	 if (!intel->has_separate_stencil)
	    fb->_Status = GL_FRAMEBUFFER_UNSUPPORTED_EXT;
	 if (stencil_mt->format != MESA_FORMAT_S8)
	    fb->_Status = GL_FRAMEBUFFER_UNSUPPORTED_EXT;
      }
   }

   for (i = 0; i < Elements(fb->Attachment); i++) {
      struct gl_renderbuffer *rb;
      struct intel_renderbuffer *irb;

      if (fb->Attachment[i].Type == GL_NONE)
	 continue;

      /* A supported attachment will have a Renderbuffer set either
       * from being a Renderbuffer or being a texture that got the
       * intel_wrap_texture() treatment.
       */
      rb = fb->Attachment[i].Renderbuffer;
      if (rb == NULL) {
	 DBG("attachment without renderbuffer\n");
	 fb->_Status = GL_FRAMEBUFFER_UNSUPPORTED_EXT;
	 continue;
      }

      irb = intel_renderbuffer(rb);
      if (irb == NULL) {
	 DBG("software rendering renderbuffer\n");
	 fb->_Status = GL_FRAMEBUFFER_UNSUPPORTED_EXT;
	 continue;
      }

      if (!intel->vtbl.render_target_supported(intel, irb->Base.Format)) {
	 DBG("Unsupported HW texture/renderbuffer format attached: %s\n",
	     _mesa_get_format_name(irb->Base.Format));
	 fb->_Status = GL_FRAMEBUFFER_UNSUPPORTED_EXT;
      }

#ifdef I915
      if (!intel_span_supports_format(irb->Base.Format)) {
	 DBG("Unsupported swrast texture/renderbuffer format attached: %s\n",
	     _mesa_get_format_name(irb->Base.Format));
	 fb->_Status = GL_FRAMEBUFFER_UNSUPPORTED_EXT;
      }
#endif
   }
}

/**
 * Try to do a glBlitFramebuffer using glCopyTexSubImage2D
 * We can do this when the dst renderbuffer is actually a texture and
 * there is no scaling, mirroring or scissoring.
 *
 * \return new buffer mask indicating the buffers left to blit using the
 *         normal path.
 */
static GLbitfield
intel_blit_framebuffer_copy_tex_sub_image(struct gl_context *ctx,
                                          GLint srcX0, GLint srcY0,
                                          GLint srcX1, GLint srcY1,
                                          GLint dstX0, GLint dstY0,
                                          GLint dstX1, GLint dstY1,
                                          GLbitfield mask, GLenum filter)
{
   if (mask & GL_COLOR_BUFFER_BIT) {
      const struct gl_framebuffer *drawFb = ctx->DrawBuffer;
      const struct gl_framebuffer *readFb = ctx->ReadBuffer;
      const struct gl_renderbuffer_attachment *drawAtt =
         &drawFb->Attachment[drawFb->_ColorDrawBufferIndexes[0]];

      /* If the source and destination are the same size with no
         mirroring, the rectangles are within the size of the
         texture and there is no scissor then we can use
         glCopyTexSubimage2D to implement the blit. This will end
         up as a fast hardware blit on some drivers */
      if (drawAtt && drawAtt->Texture &&
          srcX0 - srcX1 == dstX0 - dstX1 &&
          srcY0 - srcY1 == dstY0 - dstY1 &&
          srcX1 >= srcX0 &&
          srcY1 >= srcY0 &&
          srcX0 >= 0 && srcX1 <= readFb->Width &&
          srcY0 >= 0 && srcY1 <= readFb->Height &&
          dstX0 >= 0 && dstX1 <= drawFb->Width &&
          dstY0 >= 0 && dstY1 <= drawFb->Height &&
          !ctx->Scissor.Enabled) {
         const struct gl_texture_object *texObj = drawAtt->Texture;
         const GLuint dstLevel = drawAtt->TextureLevel;
         const GLenum target = texObj->Target;

         struct gl_texture_image *texImage =
            _mesa_select_tex_image(ctx, texObj, target, dstLevel);

         if (intel_copy_texsubimage(intel_context(ctx),
                                    intel_texture_image(texImage),
                                    dstX0, dstY0,
                                    srcX0, srcY0,
                                    srcX1 - srcX0, /* width */
                                    srcY1 - srcY0))
            mask &= ~GL_COLOR_BUFFER_BIT;
      }
   }

   return mask;
}

static void
intel_blit_framebuffer(struct gl_context *ctx,
                       GLint srcX0, GLint srcY0, GLint srcX1, GLint srcY1,
                       GLint dstX0, GLint dstY0, GLint dstX1, GLint dstY1,
                       GLbitfield mask, GLenum filter)
{
   /* Try faster, glCopyTexSubImage2D approach first which uses the BLT. */
   mask = intel_blit_framebuffer_copy_tex_sub_image(ctx,
                                                    srcX0, srcY0, srcX1, srcY1,
                                                    dstX0, dstY0, dstX1, dstY1,
                                                    mask, filter);
   if (mask == 0x0)
      return;

   _mesa_meta_BlitFramebuffer(ctx,
                              srcX0, srcY0, srcX1, srcY1,
                              dstX0, dstY0, dstX1, dstY1,
                              mask, filter);
}

void
intel_renderbuffer_set_needs_hiz_resolve(struct intel_renderbuffer *irb)
{
   if (irb->mt) {
      intel_miptree_slice_set_needs_hiz_resolve(irb->mt,
                                                irb->mt_level,
                                                irb->mt_layer);
   }
}

void
intel_renderbuffer_set_needs_depth_resolve(struct intel_renderbuffer *irb)
{
   if (irb->mt) {
      intel_miptree_slice_set_needs_depth_resolve(irb->mt,
                                                  irb->mt_level,
                                                  irb->mt_layer);
   }
}

bool
intel_renderbuffer_resolve_hiz(struct intel_context *intel,
			       struct intel_renderbuffer *irb)
{
   if (irb->mt)
      return intel_miptree_slice_resolve_hiz(intel,
                                             irb->mt,
                                             irb->mt_level,
                                             irb->mt_layer);

   return false;
}

bool
intel_renderbuffer_resolve_depth(struct intel_context *intel,
				 struct intel_renderbuffer *irb)
{
   if (irb->mt)
      return intel_miptree_slice_resolve_depth(intel,
                                               irb->mt,
                                               irb->mt_level,
                                               irb->mt_layer);

   return false;
}

/**
 * Do one-time context initializations related to GL_EXT_framebuffer_object.
 * Hook in device driver functions.
 */
void
intel_fbo_init(struct intel_context *intel)
{
   intel->ctx.Driver.NewFramebuffer = intel_new_framebuffer;
   intel->ctx.Driver.NewRenderbuffer = intel_new_renderbuffer;
   intel->ctx.Driver.MapRenderbuffer = intel_map_renderbuffer;
   intel->ctx.Driver.UnmapRenderbuffer = intel_unmap_renderbuffer;
   intel->ctx.Driver.BindFramebuffer = intel_bind_framebuffer;
   intel->ctx.Driver.FramebufferRenderbuffer = intel_framebuffer_renderbuffer;
   intel->ctx.Driver.RenderTexture = intel_render_texture;
   intel->ctx.Driver.FinishRenderTexture = intel_finish_render_texture;
   intel->ctx.Driver.ResizeBuffers = intel_resize_buffers;
   intel->ctx.Driver.ValidateFramebuffer = intel_validate_framebuffer;
   intel->ctx.Driver.BlitFramebuffer = intel_blit_framebuffer;

#if FEATURE_OES_EGL_image
   intel->ctx.Driver.EGLImageTargetRenderbufferStorage =
      intel_image_target_renderbuffer_storage;
#endif   
}
