/**************************************************************************
 *
 * Copyright 2009 Younes Manton.
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

#include "pipe/p_compiler.h"
#include "pipe/p_context.h"

#include "util/u_memory.h"
#include "util/u_draw.h"
#include "util/u_surface.h"
#include "util/u_sampler.h"

#include "tgsi/tgsi_ureg.h"

#include "vl_csc.h"
#include "vl_types.h"
#include "vl_compositor.h"

#define MIN_DIRTY (0)
#define MAX_DIRTY (1 << 15)

typedef float csc_matrix[16];

static void *
create_vert_shader(struct vl_compositor *c)
{
   struct ureg_program *shader;
   struct ureg_src vpos, vtex;
   struct ureg_dst o_vpos, o_vtex;

   shader = ureg_create(TGSI_PROCESSOR_VERTEX);
   if (!shader)
      return false;

   vpos = ureg_DECL_vs_input(shader, 0);
   vtex = ureg_DECL_vs_input(shader, 1);
   o_vpos = ureg_DECL_output(shader, TGSI_SEMANTIC_POSITION, 0);
   o_vtex = ureg_DECL_output(shader, TGSI_SEMANTIC_GENERIC, 1);

   /*
    * o_vpos = vpos
    * o_vtex = vtex
    */
   ureg_MOV(shader, o_vpos, vpos);
   ureg_MOV(shader, o_vtex, vtex);

   ureg_END(shader);

   return ureg_create_shader_and_destroy(shader, c->pipe);
}

static void *
create_frag_shader_video_buffer(struct vl_compositor *c, unsigned planes)
{
   struct ureg_program *shader;
   struct ureg_src tc;
   struct ureg_src csc[3];
   struct ureg_src sampler[3];
   struct ureg_dst texel;
   struct ureg_dst fragment;
   unsigned i;

   shader = ureg_create(TGSI_PROCESSOR_FRAGMENT);
   if (!shader)
      return false;

   tc = ureg_DECL_fs_input(shader, TGSI_SEMANTIC_GENERIC, 1, TGSI_INTERPOLATE_LINEAR);
   for (i = 0; i < 3; ++i) {
      csc[i] = ureg_DECL_constant(shader, i);
      if (i < planes)
         sampler[i] = ureg_DECL_sampler(shader, i);
   }
   texel = ureg_DECL_temporary(shader);
   fragment = ureg_DECL_output(shader, TGSI_SEMANTIC_COLOR, 0);

   /*
    * texel.xyz = tex(tc, sampler[i])
    * fragment = csc * texel
    */
   if (planes == 2) {
      ureg_TEX(shader, ureg_writemask(texel, TGSI_WRITEMASK_X), TGSI_TEXTURE_2D, tc, sampler[0]);
      ureg_TEX(shader, ureg_writemask(texel, TGSI_WRITEMASK_YZ), TGSI_TEXTURE_2D, tc, sampler[1]);
   } else {
      for (i = 0; i < 3; ++i)
         ureg_TEX(shader, ureg_writemask(texel, TGSI_WRITEMASK_X << i), TGSI_TEXTURE_2D, tc, sampler[i]);
   }

   ureg_MOV(shader, ureg_writemask(texel, TGSI_WRITEMASK_W), ureg_imm1f(shader, 1.0f));

   for (i = 0; i < 3; ++i)
      ureg_DP4(shader, ureg_writemask(fragment, TGSI_WRITEMASK_X << i), csc[i], ureg_src(texel));

   ureg_MOV(shader, ureg_writemask(fragment, TGSI_WRITEMASK_W), ureg_imm1f(shader, 1.0f));

   ureg_release_temporary(shader, texel);
   ureg_END(shader);

   return ureg_create_shader_and_destroy(shader, c->pipe);
}

static struct ureg_dst
calc_line(struct ureg_program *shader)
{
   struct ureg_dst tmp;
   struct ureg_src pos;

   tmp = ureg_DECL_temporary(shader);

   pos = ureg_DECL_fs_input(shader, TGSI_SEMANTIC_POSITION, 0, TGSI_INTERPOLATE_LINEAR);

   /*
    * tmp.y = fraction(pos.y * .5) >= 0.5 ? 1 : 0
    */
   ureg_MUL(shader, ureg_writemask(tmp, TGSI_WRITEMASK_Y), pos, ureg_imm1f(shader, 0.5f));
   ureg_FRC(shader, ureg_writemask(tmp, TGSI_WRITEMASK_Y), ureg_src(tmp));
   ureg_SGE(shader, ureg_writemask(tmp, TGSI_WRITEMASK_Y), ureg_src(tmp), ureg_imm1f(shader, 0.5f));

   return tmp;
}

/* Deinterlace NV12 or YV12 to a temporary video buffer
 */

static void *
create_frag_shader_deint_planar_weave(struct vl_compositor *c, unsigned luma, unsigned comps)
{
   struct ureg_program *shader;
   struct ureg_src tc, sampler[4];
   struct ureg_dst field, fragment, swizcolor;
   unsigned label, writemask;
   if (luma)
      writemask = TGSI_WRITEMASK_X;
   else if (comps == 2)
      writemask = TGSI_WRITEMASK_YZ;
   else
      writemask = TGSI_WRITEMASK_Y;

   shader = ureg_create(TGSI_PROCESSOR_FRAGMENT);
   if (!shader)
      return false;

   tc = ureg_DECL_fs_input(shader, TGSI_SEMANTIC_GENERIC, 1, TGSI_INTERPOLATE_LINEAR);
   fragment = ureg_DECL_output(shader, TGSI_SEMANTIC_COLOR, 0);
   sampler[0] = ureg_DECL_sampler(shader, 0);
   sampler[1] = ureg_DECL_sampler(shader, 1);
   if (!luma && comps == 1) {
      sampler[2] = ureg_DECL_sampler(shader, 2);
      sampler[3] = ureg_DECL_sampler(shader, 3);
   }

   field = calc_line(shader);
   swizcolor = ureg_DECL_temporary(shader);

   /* field.y = fraction(coord/2) >= .5 (from vl_mc.c)
    *
    * if (field.y)
    *   swiz = sampler[bottom];
    * else
    *   swiz = sampler[top];
    *
    * if (LUMA)
    *    color.x = swiz;
    * else
    *    color.xy = swiz.yz;
    */

   ureg_IF(shader, ureg_scalar(ureg_src(field), TGSI_SWIZZLE_Y), &label);
      ureg_TEX(shader, ureg_writemask(swizcolor, writemask), TGSI_TEXTURE_2D, tc, sampler[1]);
      if (!luma && comps == 1)
         ureg_TEX(shader, ureg_writemask(swizcolor, TGSI_WRITEMASK_Z), TGSI_TEXTURE_2D, tc, sampler[3]);
      ureg_fixup_label(shader, label, ureg_get_instruction_number(shader));
   ureg_ELSE(shader, &label);
      ureg_TEX(shader, ureg_writemask(swizcolor, writemask), TGSI_TEXTURE_2D, tc, sampler[0]);
      if (!luma && comps == 1)
         ureg_TEX(shader, ureg_writemask(swizcolor, TGSI_WRITEMASK_Z), TGSI_TEXTURE_2D, tc, sampler[2]);
      ureg_fixup_label(shader, label, ureg_get_instruction_number(shader));
   ureg_ENDIF(shader);

   if (luma)
      ureg_MOV(shader, ureg_writemask(fragment, writemask), ureg_src(swizcolor));
   else
      ureg_MOV(shader, ureg_writemask(fragment, TGSI_WRITEMASK_XY),
               ureg_swizzle(ureg_src(swizcolor), TGSI_SWIZZLE_Y, TGSI_SWIZZLE_Z, TGSI_SWIZZLE_Z, TGSI_SWIZZLE_Z));

   ureg_release_temporary(shader, swizcolor);
   ureg_release_temporary(shader, field);
   ureg_END(shader);
   return ureg_create_shader_and_destroy(shader, c->pipe);
}

static void *
create_frag_shader_palette(struct vl_compositor *c, bool include_cc)
{
   struct ureg_program *shader;
   struct ureg_src csc[3];
   struct ureg_src tc;
   struct ureg_src sampler;
   struct ureg_src palette;
   struct ureg_dst texel;
   struct ureg_dst fragment;
   unsigned i;

   shader = ureg_create(TGSI_PROCESSOR_FRAGMENT);
   if (!shader)
      return false;

   for (i = 0; include_cc && i < 3; ++i)
      csc[i] = ureg_DECL_constant(shader, i);

   tc = ureg_DECL_fs_input(shader, TGSI_SEMANTIC_GENERIC, 1, TGSI_INTERPOLATE_LINEAR);
   sampler = ureg_DECL_sampler(shader, 0);
   palette = ureg_DECL_sampler(shader, 1);

   texel = ureg_DECL_temporary(shader);
   fragment = ureg_DECL_output(shader, TGSI_SEMANTIC_COLOR, 0);

   /*
    * texel = tex(tc, sampler)
    * fragment.xyz = tex(texel, palette) * csc
    * fragment.a = texel.a
    */
   ureg_TEX(shader, texel, TGSI_TEXTURE_2D, tc, sampler);
   ureg_MOV(shader, ureg_writemask(fragment, TGSI_WRITEMASK_W), ureg_src(texel));

   if (include_cc) {
      ureg_TEX(shader, texel, TGSI_TEXTURE_1D, ureg_src(texel), palette);
      for (i = 0; i < 3; ++i)
         ureg_DP4(shader, ureg_writemask(fragment, TGSI_WRITEMASK_X << i), csc[i], ureg_src(texel));
   } else {
      ureg_TEX(shader, ureg_writemask(fragment, TGSI_WRITEMASK_XYZ),
               TGSI_TEXTURE_1D, ureg_src(texel), palette);
   }

   ureg_release_temporary(shader, texel);
   ureg_END(shader);

   return ureg_create_shader_and_destroy(shader, c->pipe);
}

static void *
create_frag_shader_rgba(struct vl_compositor *c)
{
   struct ureg_program *shader;
   struct ureg_src tc;
   struct ureg_src sampler;
   struct ureg_dst fragment;

   shader = ureg_create(TGSI_PROCESSOR_FRAGMENT);
   if (!shader)
      return false;

   tc = ureg_DECL_fs_input(shader, TGSI_SEMANTIC_GENERIC, 1, TGSI_INTERPOLATE_LINEAR);
   sampler = ureg_DECL_sampler(shader, 0);
   fragment = ureg_DECL_output(shader, TGSI_SEMANTIC_COLOR, 0);

   /*
    * fragment = tex(tc, sampler)
    */
   ureg_TEX(shader, fragment, TGSI_TEXTURE_2D, tc, sampler);
   ureg_END(shader);

   return ureg_create_shader_and_destroy(shader, c->pipe);
}

static bool
init_shaders(struct vl_compositor *c)
{
   assert(c);

   c->vs = create_vert_shader(c);
   if (!c->vs) {
      debug_printf("Unable to create vertex shader.\n");
      return false;
   }

   c->fs_palette.yuv = create_frag_shader_palette(c, true);
   if (!c->fs_palette.yuv) {
      debug_printf("Unable to create YUV-Palette-to-RGB fragment shader.\n");
      return false;
   }

   c->fs_palette.rgb = create_frag_shader_palette(c, false);
   if (!c->fs_palette.rgb) {
      debug_printf("Unable to create RGB-Palette-to-RGB fragment shader.\n");
      return false;
   }

   c->fs_rgba = create_frag_shader_rgba(c);
   if (!c->fs_rgba) {
      debug_printf("Unable to create RGB-to-RGB fragment shader.\n");
      return false;
   }

   return true;
}

static void cleanup_shaders(struct vl_compositor *c)
{
   assert(c);

   c->pipe->delete_vs_state(c->pipe, c->vs);
   c->pipe->delete_fs_state(c->pipe, c->fs_palette.yuv);
   c->pipe->delete_fs_state(c->pipe, c->fs_palette.rgb);
   c->pipe->delete_fs_state(c->pipe, c->fs_rgba);
}

static bool
init_pipe_state(struct vl_compositor *c)
{
   struct pipe_rasterizer_state rast;
   struct pipe_sampler_state sampler;
   struct pipe_blend_state blend;
   struct pipe_depth_stencil_alpha_state dsa;
   unsigned i;

   assert(c);

   c->fb_state.nr_cbufs = 1;
   c->fb_state.zsbuf = NULL;

   c->viewport.scale[2] = 1;
   c->viewport.scale[3] = 1;
   c->viewport.translate[2] = 0;
   c->viewport.translate[3] = 0;

   memset(&sampler, 0, sizeof(sampler));
   sampler.wrap_s = PIPE_TEX_WRAP_CLAMP_TO_EDGE;
   sampler.wrap_t = PIPE_TEX_WRAP_CLAMP_TO_EDGE;
   sampler.wrap_r = PIPE_TEX_WRAP_CLAMP_TO_EDGE;
   sampler.min_img_filter = PIPE_TEX_FILTER_LINEAR;
   sampler.min_mip_filter = PIPE_TEX_MIPFILTER_NONE;
   sampler.mag_img_filter = PIPE_TEX_FILTER_LINEAR;
   sampler.compare_mode = PIPE_TEX_COMPARE_NONE;
   sampler.compare_func = PIPE_FUNC_ALWAYS;
   sampler.normalized_coords = 1;

   c->sampler_linear = c->pipe->create_sampler_state(c->pipe, &sampler);

   sampler.min_img_filter = PIPE_TEX_FILTER_NEAREST;
   sampler.mag_img_filter = PIPE_TEX_FILTER_NEAREST;
   c->sampler_nearest = c->pipe->create_sampler_state(c->pipe, &sampler);

   memset(&blend, 0, sizeof blend);
   blend.independent_blend_enable = 0;
   blend.rt[0].blend_enable = 0;
   blend.logicop_enable = 0;
   blend.logicop_func = PIPE_LOGICOP_CLEAR;
   blend.rt[0].colormask = PIPE_MASK_RGBA;
   blend.dither = 0;
   c->blend_clear = c->pipe->create_blend_state(c->pipe, &blend);

   blend.rt[0].blend_enable = 1;
   blend.rt[0].rgb_func = PIPE_BLEND_ADD;
   blend.rt[0].rgb_src_factor = PIPE_BLENDFACTOR_SRC_ALPHA;
   blend.rt[0].rgb_dst_factor = PIPE_BLENDFACTOR_INV_SRC_ALPHA;
   blend.rt[0].alpha_func = PIPE_BLEND_ADD;
   blend.rt[0].alpha_src_factor = PIPE_BLENDFACTOR_ONE;
   blend.rt[0].alpha_dst_factor = PIPE_BLENDFACTOR_ONE;
   c->blend_add = c->pipe->create_blend_state(c->pipe, &blend);

   memset(&rast, 0, sizeof rast);
   rast.flatshade = 1;
   rast.front_ccw = 1;
   rast.cull_face = PIPE_FACE_NONE;
   rast.fill_back = PIPE_POLYGON_MODE_FILL;
   rast.fill_front = PIPE_POLYGON_MODE_FILL;
   rast.scissor = 1;
   rast.line_width = 1;
   rast.point_size_per_vertex = 1;
   rast.offset_units = 1;
   rast.offset_scale = 1;
   rast.gl_rasterization_rules = 1;

   c->rast = c->pipe->create_rasterizer_state(c->pipe, &rast);

   memset(&dsa, 0, sizeof dsa);
   dsa.depth.enabled = 0;
   dsa.depth.writemask = 0;
   dsa.depth.func = PIPE_FUNC_ALWAYS;
   for (i = 0; i < 2; ++i) {
      dsa.stencil[i].enabled = 0;
      dsa.stencil[i].func = PIPE_FUNC_ALWAYS;
      dsa.stencil[i].fail_op = PIPE_STENCIL_OP_KEEP;
      dsa.stencil[i].zpass_op = PIPE_STENCIL_OP_KEEP;
      dsa.stencil[i].zfail_op = PIPE_STENCIL_OP_KEEP;
      dsa.stencil[i].valuemask = 0;
      dsa.stencil[i].writemask = 0;
   }
   dsa.alpha.enabled = 0;
   dsa.alpha.func = PIPE_FUNC_ALWAYS;
   dsa.alpha.ref_value = 0;
   c->dsa = c->pipe->create_depth_stencil_alpha_state(c->pipe, &dsa);
   c->pipe->bind_depth_stencil_alpha_state(c->pipe, c->dsa);

   return true;
}

static void cleanup_pipe_state(struct vl_compositor *c)
{
   assert(c);

   /* Asserted in softpipe_delete_fs_state() for some reason */
   c->pipe->bind_vs_state(c->pipe, NULL);
   c->pipe->bind_fs_state(c->pipe, NULL);

   c->pipe->delete_depth_stencil_alpha_state(c->pipe, c->dsa);
   c->pipe->delete_sampler_state(c->pipe, c->sampler_linear);
   c->pipe->delete_sampler_state(c->pipe, c->sampler_nearest);
   c->pipe->delete_blend_state(c->pipe, c->blend_clear);
   c->pipe->delete_blend_state(c->pipe, c->blend_add);
   c->pipe->delete_rasterizer_state(c->pipe, c->rast);
}

static bool
create_vertex_buffer(struct vl_compositor *c)
{
   assert(c);

   pipe_resource_reference(&c->vertex_buf.buffer, NULL);
   c->vertex_buf.buffer = pipe_buffer_create
   (
      c->pipe->screen,
      PIPE_BIND_VERTEX_BUFFER,
      PIPE_USAGE_STREAM,
      sizeof(struct vertex4f) * VL_COMPOSITOR_MAX_LAYERS * 4
   );

   return c->vertex_buf.buffer != NULL;
}

static bool
init_buffers(struct vl_compositor *c)
{
   struct pipe_vertex_element vertex_elems[2];

   assert(c);

   /*
    * Create our vertex buffer and vertex buffer elements
    */
   c->vertex_buf.stride = sizeof(struct vertex4f);
   c->vertex_buf.buffer_offset = 0;
   create_vertex_buffer(c);

   vertex_elems[0].src_offset = 0;
   vertex_elems[0].instance_divisor = 0;
   vertex_elems[0].vertex_buffer_index = 0;
   vertex_elems[0].src_format = PIPE_FORMAT_R32G32_FLOAT;
   vertex_elems[1].src_offset = sizeof(struct vertex2f);
   vertex_elems[1].instance_divisor = 0;
   vertex_elems[1].vertex_buffer_index = 0;
   vertex_elems[1].src_format = PIPE_FORMAT_R32G32_FLOAT;
   c->vertex_elems_state = c->pipe->create_vertex_elements_state(c->pipe, 2, vertex_elems);

   /*
    * Create our fragment shader's constant buffer
    * Const buffer contains the color conversion matrix and bias vectors
    */
   /* XXX: Create with IMMUTABLE/STATIC... although it does change every once in a long while... */
   c->csc_matrix = pipe_buffer_create
   (
      c->pipe->screen,
      PIPE_BIND_CONSTANT_BUFFER,
      PIPE_USAGE_STATIC,
      sizeof(csc_matrix)
   );

   return true;
}

static void
cleanup_buffers(struct vl_compositor *c)
{
   assert(c);

   c->pipe->delete_vertex_elements_state(c->pipe, c->vertex_elems_state);
   pipe_resource_reference(&c->vertex_buf.buffer, NULL);
   pipe_resource_reference(&c->csc_matrix, NULL);
}

static INLINE struct pipe_video_rect
default_rect(struct vl_compositor_layer *layer)
{
   struct pipe_resource *res = layer->sampler_views[0]->texture;
   struct pipe_video_rect rect = { 0, 0, res->width0, res->height0 };
   return rect;
}

static INLINE struct vertex2f
calc_topleft(struct vertex2f size, struct pipe_video_rect rect)
{
   struct vertex2f res = { rect.x / size.x, rect.y / size.y };
   return res;
}

static INLINE struct vertex2f
calc_bottomright(struct vertex2f size, struct pipe_video_rect rect)
{
   struct vertex2f res = { (rect.x + rect.w) / size.x, (rect.y + rect.h) / size.y };
   return res;
}

static INLINE void
calc_src_and_dst(struct vl_compositor_layer *layer, unsigned width, unsigned height,
                 struct pipe_video_rect src, struct pipe_video_rect dst)
{
   struct vertex2f size =  { width, height };

   layer->src.tl = calc_topleft(size, src);
   layer->src.br = calc_bottomright(size, src);
   layer->dst.tl = calc_topleft(size, dst);
   layer->dst.br = calc_bottomright(size, dst);
}

static void
gen_rect_verts(struct vertex4f *vb, struct vl_compositor_layer *layer)
{
   assert(vb && layer);

   vb[0].x = layer->dst.tl.x;
   vb[0].y = layer->dst.tl.y;
   vb[0].z = layer->src.tl.x;
   vb[0].w = layer->src.tl.y;

   vb[1].x = layer->dst.br.x;
   vb[1].y = layer->dst.tl.y;
   vb[1].z = layer->src.br.x;
   vb[1].w = layer->src.tl.y;

   vb[2].x = layer->dst.br.x;
   vb[2].y = layer->dst.br.y;
   vb[2].z = layer->src.br.x;
   vb[2].w = layer->src.br.y;

   vb[3].x = layer->dst.tl.x;
   vb[3].y = layer->dst.br.y;
   vb[3].z = layer->src.tl.x;
   vb[3].w = layer->src.br.y;
}

static INLINE struct u_rect
calc_drawn_area(struct vl_compositor *c, struct vl_compositor_layer *layer)
{
   struct u_rect result;

   // scale
   result.x0 = layer->dst.tl.x * c->viewport.scale[0] + c->viewport.translate[0];
   result.y0 = layer->dst.tl.y * c->viewport.scale[1] + c->viewport.translate[1];
   result.x1 = layer->dst.br.x * c->viewport.scale[0] + c->viewport.translate[0];
   result.y1 = layer->dst.br.y * c->viewport.scale[1] + c->viewport.translate[1];

   // and clip
   result.x0 = MAX2(result.x0, c->scissor.minx);
   result.y0 = MAX2(result.y0, c->scissor.miny);
   result.x1 = MIN2(result.x1, c->scissor.maxx);
   result.y1 = MIN2(result.y1, c->scissor.maxy);
   return result;
}

static void
gen_vertex_data(struct vl_compositor *c, struct u_rect *dirty)
{
   struct vertex4f *vb;
   struct pipe_transfer *buf_transfer;
   unsigned i;

   assert(c);

   vb = pipe_buffer_map(c->pipe, c->vertex_buf.buffer,
                        PIPE_TRANSFER_WRITE | PIPE_TRANSFER_DISCARD | PIPE_TRANSFER_DONTBLOCK,
                        &buf_transfer);

   if (!vb) {
      // If buffer is still locked from last draw create a new one
      create_vertex_buffer(c);
      vb = pipe_buffer_map(c->pipe, c->vertex_buf.buffer,
                           PIPE_TRANSFER_WRITE | PIPE_TRANSFER_DISCARD,
                           &buf_transfer);
   }

   for (i = 0; i < VL_COMPOSITOR_MAX_LAYERS; i++) {
      if (c->used_layers & (1 << i)) {
         struct vl_compositor_layer *layer = &c->layers[i];
         gen_rect_verts(vb, layer);
         vb += 4;

         if (dirty && layer->clearing) {
            struct u_rect drawn = calc_drawn_area(c, layer);
            if (
             dirty->x0 >= drawn.x0 &&
             dirty->y0 >= drawn.y0 &&
             dirty->x1 <= drawn.x1 &&
             dirty->y1 <= drawn.y1) {

               // We clear the dirty area anyway, no need for clear_render_target
               dirty->x0 = dirty->y0 = MAX_DIRTY;
               dirty->x1 = dirty->y1 = MIN_DIRTY;
            }
         }
      }
   }

   pipe_buffer_unmap(c->pipe, buf_transfer);
}

static void
draw_layers(struct vl_compositor *c, struct u_rect *dirty)
{
   unsigned vb_index, i;

   assert(c);

   for (i = 0, vb_index = 0; i < VL_COMPOSITOR_MAX_LAYERS; ++i) {
      if (c->used_layers & (1 << i)) {
         struct vl_compositor_layer *layer = &c->layers[i];
         struct pipe_sampler_view **samplers = &layer->sampler_views[0];
         unsigned num_sampler_views = !samplers[1] ? 1 : !samplers[2] ? 2 : 3;

         c->pipe->bind_blend_state(c->pipe, layer->blend);
         c->pipe->bind_fs_state(c->pipe, layer->fs);
         c->pipe->bind_fragment_sampler_states(c->pipe, num_sampler_views, layer->samplers);
         c->pipe->set_fragment_sampler_views(c->pipe, num_sampler_views, samplers);
         util_draw_arrays(c->pipe, PIPE_PRIM_QUADS, vb_index * 4, 4);
         vb_index++;

         if (dirty) {
            // Remember the currently drawn area as dirty for the next draw command
            struct u_rect drawn = calc_drawn_area(c, layer);
            dirty->x0 = MIN2(drawn.x0, dirty->x0);
            dirty->y0 = MIN2(drawn.y0, dirty->y0);
            dirty->x1 = MAX2(drawn.x1, dirty->x1);
            dirty->y1 = MAX2(drawn.y1, dirty->y1);
         }
      }
   }
}

void
vl_compositor_reset_dirty_area(struct u_rect *dirty)
{
   assert(dirty);

   dirty->x0 = dirty->y0 = MIN_DIRTY;
   dirty->x1 = dirty->y1 = MAX_DIRTY;
}

void
vl_compositor_set_clear_color(struct vl_compositor *c, union pipe_color_union *color)
{
   assert(c);

   c->clear_color = *color;
}

void
vl_compositor_get_clear_color(struct vl_compositor *c, union pipe_color_union *color)
{
   assert(c);
   assert(color);

   *color = c->clear_color;
}

void
vl_compositor_clear_layers(struct vl_compositor *c)
{
   unsigned i, j;

   assert(c);

   c->used_layers = 0;
   for ( i = 0; i < VL_COMPOSITOR_MAX_LAYERS; ++i) {
      c->layers[i].clearing = i ? false : true;
      c->layers[i].blend = i ? c->blend_add : c->blend_clear;
      c->layers[i].fs = NULL;
      for ( j = 0; j < 3; j++)
         pipe_sampler_view_reference(&c->layers[i].sampler_views[j], NULL);
   }
}

static void
cleanup_video(struct vl_compositor *c)
{
   unsigned i;
   for (i = 0; i < 3; ++i) {
      if (!c->fs_weave[i])
         continue;
      c->pipe->delete_fs_state(c->pipe, c->fs_weave[i]);
   }
   if (c->fs_video_buffer2)
      c->pipe->delete_fs_state(c->pipe, c->fs_video_buffer2);
   if (c->fs_video_buffer3)
      c->pipe->delete_fs_state(c->pipe, c->fs_video_buffer3);
   pipe_surface_reference(&c->video_surf[0], NULL);
   pipe_surface_reference(&c->video_surf[1], NULL);
   pipe_sampler_view_reference(&c->video_sv[0], NULL);
   pipe_sampler_view_reference(&c->video_sv[1], NULL);
   pipe_resource_reference(&c->video_y, NULL);
   pipe_resource_reference(&c->video_cbcr, NULL);
}

void
vl_compositor_cleanup(struct vl_compositor *c)
{
   assert(c);

   vl_compositor_clear_layers(c);
   cleanup_buffers(c);
   cleanup_shaders(c);
   cleanup_pipe_state(c);
   cleanup_video(c);
}

void
vl_compositor_set_csc_matrix(struct vl_compositor *c, const float matrix[16])
{
   struct pipe_transfer *buf_transfer;

   assert(c);

   memcpy
   (
      pipe_buffer_map(c->pipe, c->csc_matrix,
                      PIPE_TRANSFER_WRITE | PIPE_TRANSFER_DISCARD,
                      &buf_transfer),
      matrix,
      sizeof(csc_matrix)
   );

   pipe_buffer_unmap(c->pipe, buf_transfer);
}

void
vl_compositor_set_layer_blend(struct vl_compositor *c,
                              unsigned layer, void *blend,
                              bool is_clearing)
{
   assert(c && blend);

   assert(layer < VL_COMPOSITOR_MAX_LAYERS);

   c->layers[layer].clearing = is_clearing;
   c->layers[layer].blend = blend;
}


static void gen_vertex_data_video(struct vl_compositor *c) {
   struct vertex4f *vb;
   struct pipe_transfer *buf_transfer;
   vb = pipe_buffer_map(c->pipe, c->vertex_buf.buffer,
                        PIPE_TRANSFER_WRITE | PIPE_TRANSFER_DISCARD | PIPE_TRANSFER_DONTBLOCK,
                        &buf_transfer);

   if (!vb) {
      // If buffer is still locked from last draw create a new one
      create_vertex_buffer(c);
      vb = pipe_buffer_map(c->pipe, c->vertex_buf.buffer,
                           PIPE_TRANSFER_WRITE | PIPE_TRANSFER_DISCARD,
                           &buf_transfer);
   }
   vb[0].x = 0.f;
   vb[0].y = 0.f;
   vb[0].z = 0.f;
   vb[0].w = 0.f;

   vb[1].x = 0.f;
   vb[1].y = 65535.f;
   vb[1].z = 0.f;
   vb[1].w = 65535.f;

   vb[2].x = 65535.f;
   vb[2].y = 0.f;
   vb[2].z = 65535.f;
   vb[2].w = 0.f;
   pipe_buffer_unmap(c->pipe, buf_transfer);
}

static void
vl_compositor_render_video(struct vl_compositor *c,
                           struct pipe_video_buffer *buffer)
{
   struct pipe_sampler_view **sv;
   struct pipe_scissor_state scissor;
   sv = buffer->get_sampler_view_planes(buffer, 1);
   void *samplers[4];
   unsigned i;
   for (i = 0; i < 4; ++i)
      samplers[i] = c->sampler_nearest;

   assert(c);
   gen_vertex_data_video(c);
   for (i = 0; i < 2; ++i) {
      struct pipe_surface *dst_surface = c->video_surf[i];
      assert(dst_surface);
      unsigned num_sampler_views;
      if (i == 0)
         num_sampler_views = 2;
      else
         num_sampler_views = 2 * (1 + !!sv[4]);

      c->fb_state.width = dst_surface->width;
      c->fb_state.height = dst_surface->height;
      c->fb_state.cbufs[0] = dst_surface;

      c->viewport.scale[0] = sv[2*i]->texture->width0;
      c->viewport.scale[1] = sv[2*i]->texture->height0 * 2;
      c->viewport.translate[0] = 0;
      c->viewport.translate[1] = 0;

      scissor.minx = 0;
      scissor.miny = 0;
      scissor.maxx = dst_surface->width;
      scissor.maxy = dst_surface->height;

      c->pipe->set_scissor_state(c->pipe, &scissor);
      c->pipe->set_framebuffer_state(c->pipe, &c->fb_state);
      c->pipe->set_viewport_state(c->pipe, &c->viewport);
      c->pipe->bind_vs_state(c->pipe, c->vs);
      c->pipe->set_vertex_buffers(c->pipe, 1, &c->vertex_buf);
      c->pipe->bind_vertex_elements_state(c->pipe, c->vertex_elems_state);
      c->pipe->bind_rasterizer_state(c->pipe, c->rast);

      c->pipe->bind_blend_state(c->pipe, c->blend_clear);
      if (i == 0)
         c->pipe->bind_fs_state(c->pipe, c->fs_weave[0]);
      else
         c->pipe->bind_fs_state(c->pipe, c->fs_weave[1 + !!sv[4]]);
      c->pipe->bind_fragment_sampler_states(c->pipe, num_sampler_views, samplers);
      c->pipe->set_fragment_sampler_views(c->pipe, num_sampler_views, &sv[2*i]);
      util_draw_arrays(c->pipe, PIPE_PRIM_TRIANGLES, 0, 3);
   }
}

void
vl_compositor_set_buffer_layer(struct vl_compositor *c,
                               unsigned layer,
                               struct pipe_video_buffer *buffer,
                               struct pipe_video_rect *src_rect,
                               struct pipe_video_rect *dst_rect)
{
   struct pipe_sampler_view **sampler_views;
   struct pipe_video_rect rect;
   unsigned i;

   assert(c && buffer);
   assert(c->video_w == buffer->width && c->video_h == buffer->height);

   assert(layer < VL_COMPOSITOR_MAX_LAYERS);

   c->used_layers |= 1 << layer;

   sampler_views = buffer->get_sampler_view_planes(buffer, 0);
   if (!sampler_views) {
      sampler_views = c->video_sv;
      vl_compositor_render_video(c, buffer);
      assert(!sampler_views[2]);
   }
   if (!src_rect) {
      src_rect = &rect;
      rect.x = rect.y = 0;
      rect.w = buffer->width;
      rect.h = buffer->height;
   }
   for (i = 0; i < 3; ++i) {
      c->layers[layer].samplers[i] = c->sampler_linear;
      pipe_sampler_view_reference(&c->layers[layer].sampler_views[i], sampler_views[i]);
   }
   if (sampler_views[2])
      c->layers[layer].fs = c->fs_video_buffer3;
   else
      c->layers[layer].fs = c->fs_video_buffer2;
   assert(sampler_views[1]);

   calc_src_and_dst(&c->layers[layer], buffer->width, buffer->height,
                    src_rect ? *src_rect : default_rect(&c->layers[layer]),
                    dst_rect ? *dst_rect : default_rect(&c->layers[layer]));
}

void
vl_compositor_set_palette_layer(struct vl_compositor *c,
                                unsigned layer,
                                struct pipe_sampler_view *indexes,
                                struct pipe_sampler_view *palette,
                                struct pipe_video_rect *src_rect,
                                struct pipe_video_rect *dst_rect,
                                bool include_color_conversion)
{
   assert(c && indexes && palette);

   assert(layer < VL_COMPOSITOR_MAX_LAYERS);

   c->used_layers |= 1 << layer;

   c->layers[layer].fs = include_color_conversion ?
      c->fs_palette.yuv : c->fs_palette.rgb;

   c->layers[layer].samplers[0] = c->sampler_linear;
   c->layers[layer].samplers[1] = c->sampler_nearest;
   c->layers[layer].samplers[2] = NULL;
   pipe_sampler_view_reference(&c->layers[layer].sampler_views[0], indexes);
   pipe_sampler_view_reference(&c->layers[layer].sampler_views[1], palette);
   pipe_sampler_view_reference(&c->layers[layer].sampler_views[2], NULL);
   calc_src_and_dst(&c->layers[layer], indexes->texture->width0, indexes->texture->height0,
                    src_rect ? *src_rect : default_rect(&c->layers[layer]),
                    dst_rect ? *dst_rect : default_rect(&c->layers[layer]));
}

void
vl_compositor_set_rgba_layer(struct vl_compositor *c,
                             unsigned layer,
                             struct pipe_sampler_view *rgba,
                             struct pipe_video_rect *src_rect,
                             struct pipe_video_rect *dst_rect)
{
   assert(c && rgba);

   assert(layer < VL_COMPOSITOR_MAX_LAYERS);

   c->used_layers |= 1 << layer;
   c->layers[layer].fs = c->fs_rgba;
   c->layers[layer].samplers[0] = c->sampler_linear;
   c->layers[layer].samplers[1] = NULL;
   c->layers[layer].samplers[2] = NULL;
   pipe_sampler_view_reference(&c->layers[layer].sampler_views[0], rgba);
   pipe_sampler_view_reference(&c->layers[layer].sampler_views[1], NULL);
   pipe_sampler_view_reference(&c->layers[layer].sampler_views[2], NULL);
   calc_src_and_dst(&c->layers[layer], rgba->texture->width0, rgba->texture->height0,
                    src_rect ? *src_rect : default_rect(&c->layers[layer]),
                    dst_rect ? *dst_rect : default_rect(&c->layers[layer]));
}

void
vl_compositor_render(struct vl_compositor   *c,
                     struct pipe_surface    *dst_surface,
                     struct pipe_video_rect *dst_area,
                     struct pipe_video_rect *dst_clip,
                     struct u_rect          *dirty_area)
{
   assert(c);
   assert(dst_surface);

   c->fb_state.width = dst_surface->width;
   c->fb_state.height = dst_surface->height;
   c->fb_state.cbufs[0] = dst_surface;
   
   if (dst_area) {
      c->viewport.scale[0] = dst_area->w;
      c->viewport.scale[1] = dst_area->h;
      c->viewport.translate[0] = dst_area->x;
      c->viewport.translate[1] = dst_area->y;
   } else {
      c->viewport.scale[0] = dst_surface->width;
      c->viewport.scale[1] = dst_surface->height;
      c->viewport.translate[0] = 0;
      c->viewport.translate[1] = 0;
   }

   if (dst_clip) {
      c->scissor.minx = dst_clip->x;
      c->scissor.miny = dst_clip->y;
      c->scissor.maxx = dst_clip->x + dst_clip->w;
      c->scissor.maxy = dst_clip->y + dst_clip->h;
   } else {
      c->scissor.minx = 0;
      c->scissor.miny = 0;
      c->scissor.maxx = dst_surface->width;
      c->scissor.maxy = dst_surface->height;
   }

   gen_vertex_data(c, dirty_area);

   if (dirty_area && (dirty_area->x0 < dirty_area->x1 ||
                      dirty_area->y0 < dirty_area->y1)) {

      c->pipe->clear_render_target(c->pipe, dst_surface, &c->clear_color,
                                   0, 0, dst_surface->width, dst_surface->height);
      dirty_area->x0 = dirty_area->y0 = MAX_DIRTY;
      dirty_area->x0 = dirty_area->y1 = MIN_DIRTY;
   }

   c->pipe->set_scissor_state(c->pipe, &c->scissor);
   c->pipe->set_framebuffer_state(c->pipe, &c->fb_state);
   c->pipe->set_viewport_state(c->pipe, &c->viewport);
   c->pipe->bind_vs_state(c->pipe, c->vs);
   c->pipe->set_vertex_buffers(c->pipe, 1, &c->vertex_buf);
   c->pipe->bind_vertex_elements_state(c->pipe, c->vertex_elems_state);
   c->pipe->set_constant_buffer(c->pipe, PIPE_SHADER_FRAGMENT, 0, c->csc_matrix);
   c->pipe->bind_rasterizer_state(c->pipe, c->rast);

   draw_layers(c, dirty_area);
}

bool
vl_compositor_init(struct vl_compositor *c, struct pipe_context *pipe)
{
   csc_matrix csc_matrix;

   c->pipe = pipe;

   if (!init_pipe_state(c))
      return false;

   if (!init_shaders(c)) {
      cleanup_pipe_state(c);
      return false;
   }

   if (!init_buffers(c)) {
      cleanup_shaders(c);
      cleanup_pipe_state(c);
      return false;
   }

   vl_compositor_clear_layers(c);

   vl_csc_get_matrix(VL_CSC_COLOR_STANDARD_IDENTITY, NULL, true, csc_matrix);
   vl_compositor_set_csc_matrix(c, csc_matrix);

   c->clear_color.f[0] = c->clear_color.f[1] = 0.0f;
   c->clear_color.f[2] = c->clear_color.f[3] = 0.0f;

   return true;
}

bool
vl_compositor_init_video(struct vl_compositor *c, struct pipe_context *pipe,
                         enum pipe_video_chroma_format chroma, unsigned w, unsigned h)
{
   struct pipe_resource templ;
   struct pipe_sampler_view sv_templ;
   struct pipe_surface surf_templ;
   if (!vl_compositor_init(c, pipe))
      return false;
   c->video_w = w;
   c->video_h = h;

   c->fs_video_buffer2 = create_frag_shader_video_buffer(c, 2);
   if (!c->fs_video_buffer2) {
      debug_printf("Unable to create YCbCr-to-RGB fragment shader for 2 planes.\n");
      goto fail;
   }
   c->fs_video_buffer3 = create_frag_shader_video_buffer(c, 3);
   if (!c->fs_video_buffer3) {
      debug_printf("Unable to create YCbCr-to-RGB fragment shader for 3 planes.\n");
      goto fail;
   }
   c->fs_weave[0] = create_frag_shader_deint_planar_weave(c, 1, 1);
   c->fs_weave[1] = create_frag_shader_deint_planar_weave(c, 0, 2); // CbCr woven
   c->fs_weave[2] = create_frag_shader_deint_planar_weave(c, 0, 1); // Cb, Cr separate
   if (!c->fs_weave[0] || !c->fs_weave[1] || !c->fs_weave[2]) {
      debug_printf("Unable to create weave fragment shaders.\n");
      goto fail;
   }
   memset(&templ, 0, sizeof(templ));
   templ.target = PIPE_TEXTURE_2D;
   templ.format = PIPE_FORMAT_R8_UNORM;
   templ.width0 = w;
   templ.height0 = h;
   templ.depth0 = 1;
   templ.array_size = 1;
   templ.bind = PIPE_BIND_SAMPLER_VIEW | PIPE_BIND_RENDER_TARGET;
   templ.usage = PIPE_USAGE_STATIC;
   c->video_y = pipe->screen->resource_create(pipe->screen, &templ);
   if (!c->video_y) {
      debug_printf("Could not create deinterlaced temp frame for luma\n");
      goto fail;
   }
   if (chroma != PIPE_VIDEO_CHROMA_FORMAT_444)
      templ.height0 /= 2;
   if (chroma == PIPE_VIDEO_CHROMA_FORMAT_420)
      templ.width0 /= 2;
   templ.format = PIPE_FORMAT_R8G8_UNORM;
   c->video_cbcr = pipe->screen->resource_create(pipe->screen, &templ);
   if (!c->video_cbcr) {
      debug_printf("Could not create deinterlaced temp frame for chroma\n");
      goto fail;
   }
   memset(&sv_templ, 0, sizeof(sv_templ));
   u_sampler_view_default_template(&sv_templ, c->video_y, c->video_y->format);
   sv_templ.swizzle_a = sv_templ.swizzle_b = sv_templ.swizzle_g = sv_templ.swizzle_r;
   c->video_sv[0] = pipe->create_sampler_view(pipe, c->video_y, &sv_templ);

   memset(&sv_templ, 0, sizeof(sv_templ));
   u_sampler_view_default_template(&sv_templ, c->video_cbcr, c->video_cbcr->format);
   sv_templ.swizzle_b = PIPE_SWIZZLE_GREEN;
   sv_templ.swizzle_g = PIPE_SWIZZLE_RED;
   c->video_sv[1] = pipe->create_sampler_view(pipe, c->video_cbcr, &sv_templ);
   if (!c->video_sv[0] || !c->video_sv[1]) {
      debug_printf("Could not create temp video sampler views\n");
      goto fail;
   }
   memset(&surf_templ, 0, sizeof(surf_templ));
   surf_templ.usage = PIPE_BIND_SAMPLER_VIEW | PIPE_BIND_RENDER_TARGET;
   surf_templ.format = c->video_y->format;
   c->video_surf[0] = pipe->create_surface(pipe, c->video_y, &surf_templ);
   surf_templ.format = c->video_cbcr->format;
   c->video_surf[1] = pipe->create_surface(pipe, c->video_cbcr, &surf_templ);
   if (!c->video_surf[0] || !c->video_surf[1]) {
      debug_printf("Could not create temp video surface\n");
      goto fail;
   }

   return true;

fail:
   vl_compositor_cleanup(c);
   return false;
}
