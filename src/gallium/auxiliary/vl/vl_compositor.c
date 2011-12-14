/**************************************************************************
 *
 * Copyright 2009 Younes Manton.
 * Copyright 2011 Maarten Lankhorst
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

/* Set to 1 to run a contour shader for interpolation */
#define DEBUG_CONTOUR 1

/* Set to 1 to make all non-interpolated data in the contour shader black */
#define DEBUG_CONTOUR_CHROMA 0

static const float vl_contour_luma_threshold = .1f;

/* Set to non-zero half-pixel units for finding what would be affected by bicubic resizing */
#define DEBUG_BICUBIC 0

/* There's no point in using bicubic, it's not a good interpolation method for video when downscaling
 * and for upscaling it's completely pointless until you get to a magnification of 4x or so
 */
#define USE_BICUBIC 0

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

static struct ureg_dst
calc_line(struct ureg_program *shader, struct ureg_src pos, unsigned nearest)
{
   struct ureg_dst tmp;

   tmp = ureg_DECL_temporary(shader);

   /*
    * tmp.y = fraction(pos.y * .5) >= 0.5 ? 1 : 0
    * however, for linear interpolation (chroma deinterlace) 2 pixels are required..
    */
   if (nearest)
      ureg_MUL(shader, ureg_writemask(tmp, TGSI_WRITEMASK_Y), pos, ureg_imm1f(shader, 0.5f));
   else
      ureg_MUL(shader, ureg_writemask(tmp, TGSI_WRITEMASK_Y), pos, ureg_imm1f(shader, 0.25f));
   ureg_FRC(shader, ureg_writemask(tmp, TGSI_WRITEMASK_Y), ureg_src(tmp));
   ureg_SGE(shader, ureg_writemask(tmp, TGSI_WRITEMASK_Y), ureg_src(tmp), ureg_imm1f(shader, 0.5f));

   return tmp;
}

static void *
create_frag_shader_video_buffer(struct vl_compositor *c)
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
      sampler[i] = ureg_DECL_sampler(shader, i);
   }
   texel = ureg_DECL_temporary(shader);
   fragment = ureg_DECL_output(shader, TGSI_SEMANTIC_COLOR, 0);

   /*
    * texel.xyz = tex(tc, sampler[i])
    * fragment = csc * texel
    */
   for (i = 0; i < 3; ++i)
       ureg_TEX(shader, ureg_writemask(texel, TGSI_WRITEMASK_X << i), TGSI_TEXTURE_2D, tc, sampler[i]);
   ureg_MOV(shader, ureg_writemask(texel, TGSI_WRITEMASK_W), ureg_imm1f(shader, 1.0f));

   for (i = 0; i < 3; ++i)
      ureg_DP4(shader, ureg_writemask(fragment, TGSI_WRITEMASK_X << i), csc[i], ureg_src(texel));

   ureg_MOV(shader, ureg_writemask(fragment, TGSI_WRITEMASK_W), ureg_imm1f(shader, 1.0f));

   ureg_release_temporary(shader, texel);
   ureg_END(shader);

   return ureg_create_shader_and_destroy(shader, c->pipe);
}

static void *
create_frag_shader_sobel_video(struct vl_compositor *c)
{
   struct ureg_program *shader;
   struct ureg_src tc;
   struct ureg_src csc[3];
   struct ureg_src sampler[5], invsize, size;
   struct ureg_dst texel, tmp, fragment;
   unsigned i, label;

   shader = ureg_create(TGSI_PROCESSOR_FRAGMENT);
   if (!shader)
      return false;

   tc = ureg_DECL_fs_input(shader, TGSI_SEMANTIC_GENERIC, 1, TGSI_INTERPOLATE_LINEAR);
   for (i = 0; i < 3; ++i) {
      csc[i] = ureg_DECL_constant(shader, i);
      sampler[i] = ureg_DECL_sampler(shader, i);
   }
   for (; i < 5; ++i)
      sampler[i] = ureg_DECL_sampler(shader, i);
   size = ureg_DECL_constant(shader, 4);
   invsize = ureg_DECL_constant(shader, 5);

   texel = ureg_DECL_temporary(shader);
   tmp = ureg_DECL_temporary(shader);
   fragment = ureg_DECL_output(shader, TGSI_SEMANTIC_COLOR, 0);

   ureg_TEX(shader, ureg_writemask(tmp, TGSI_WRITEMASK_X), TGSI_TEXTURE_2D, tc, sampler[3]);
   ureg_SLE(shader, ureg_writemask(tmp, TGSI_WRITEMASK_Y), ureg_scalar(ureg_src(tmp), TGSI_SWIZZLE_X), ureg_imm1f(shader, 1.f));
   ureg_SGE(shader, ureg_writemask(tmp, TGSI_WRITEMASK_X), ureg_src(tmp), ureg_imm1f(shader, vl_contour_luma_threshold));
   ureg_IF(shader, ureg_swizzle(ureg_src(tmp), TGSI_SWIZZLE_X, TGSI_SWIZZLE_Y, TGSI_SWIZZLE_Y, TGSI_SWIZZLE_Y), &label);
   {
      ureg_MUL(shader, ureg_writemask(tmp, TGSI_WRITEMASK_XY), tc,
               ureg_swizzle(size, TGSI_SWIZZLE_Z, TGSI_SWIZZLE_W, TGSI_SWIZZLE_W, TGSI_SWIZZLE_W));
      ureg_MUL(shader, ureg_writemask(tmp, TGSI_WRITEMASK_XY), ureg_src(tmp), ureg_imm2f(shader, .5f, .5f));
      ureg_FRC(shader, ureg_writemask(tmp, TGSI_WRITEMASK_XY), ureg_src(tmp));
      ureg_SGE(shader, ureg_writemask(tmp, TGSI_WRITEMASK_XY), ureg_src(tmp), ureg_imm2f(shader, .5f, .5f));
      ureg_SUB(shader, ureg_writemask(tmp, TGSI_WRITEMASK_XY), ureg_src(tmp), ureg_imm2f(shader, .5f, .5f));
      // tmp.xy = -.5 for even x / y, .5 for odd x / y, this is then multiplied
      // by absolute gradient previously calculated in the second sobel shader
      // for the chroma offset

      ureg_TEX(shader, texel, TGSI_TEXTURE_2D, tc, sampler[4]);
      ureg_MUL(shader, tmp, ureg_src(texel),
               ureg_swizzle(ureg_src(tmp), TGSI_SWIZZLE_X, TGSI_SWIZZLE_Y, TGSI_SWIZZLE_X, TGSI_SWIZZLE_Y));

      ureg_MAD(shader, tmp, ureg_src(tmp),
               ureg_swizzle(invsize, TGSI_SWIZZLE_Z, TGSI_SWIZZLE_W, TGSI_SWIZZLE_Z, TGSI_SWIZZLE_W),
               ureg_swizzle(tc, TGSI_SWIZZLE_X, TGSI_SWIZZLE_Y, TGSI_SWIZZLE_X, TGSI_SWIZZLE_Y));

      ureg_TEX(shader, ureg_writemask(texel, TGSI_WRITEMASK_X), TGSI_TEXTURE_2D, tc, sampler[0]);
      ureg_TEX(shader, ureg_writemask(texel, TGSI_WRITEMASK_Y), TGSI_TEXTURE_2D, ureg_src(tmp), sampler[1]);
      ureg_TEX(shader, ureg_writemask(texel, TGSI_WRITEMASK_Z), TGSI_TEXTURE_2D,
               ureg_swizzle(ureg_src(tmp), TGSI_SWIZZLE_Z, TGSI_SWIZZLE_W, TGSI_SWIZZLE_W, TGSI_SWIZZLE_W), sampler[2]);

//      ureg_MOV(shader, ureg_writemask(texel, TGSI_WRITEMASK_X), ureg_imm3f(shader, 0.f, .5f, .5f));
      ureg_fixup_label(shader, label, ureg_get_instruction_number(shader));
   }
   ureg_ELSE(shader, &label);
   {
      ureg_TEX(shader, ureg_writemask(texel, TGSI_WRITEMASK_X), TGSI_TEXTURE_2D, tc, sampler[0]);
      ureg_TEX(shader, ureg_writemask(texel, TGSI_WRITEMASK_Y), TGSI_TEXTURE_2D, tc, sampler[1]);
      ureg_TEX(shader, ureg_writemask(texel, TGSI_WRITEMASK_Z), TGSI_TEXTURE_2D, tc, sampler[2]);
      if (DEBUG_CONTOUR_CHROMA)
         ureg_MOV(shader, ureg_writemask(texel, TGSI_WRITEMASK_XYZ), ureg_imm3f(shader, 0.f, .5f, .5f));
      ureg_fixup_label(shader, label, ureg_get_instruction_number(shader));
   }
   ureg_ENDIF(shader);
   ureg_release_temporary(shader, tmp);

   ureg_MOV(shader, ureg_writemask(texel, TGSI_WRITEMASK_W), ureg_imm1f(shader, 1.0f));

   for (i = 0; i < 3; ++i)
      ureg_DP4(shader, ureg_writemask(fragment, TGSI_WRITEMASK_X << i), csc[i], ureg_src(texel));

   ureg_MOV(shader, ureg_writemask(fragment, TGSI_WRITEMASK_W), ureg_imm1f(shader, 1.0f));

   ureg_release_temporary(shader, texel);
   ureg_END(shader);

   return ureg_create_shader_and_destroy(shader, c->pipe);
}


/* Deinterlace or weave NV12 or YV12 to a temporary video buffer
 */

static void *
create_frag_shader_weave(struct vl_compositor *c, unsigned luma, unsigned interlaced)
{
   struct ureg_program *shader;
   struct ureg_src pos, tc, sampler[4];
   struct ureg_dst field, fragment, swizcolor;
   unsigned label, writemask, nearest;

   if (luma)
      writemask = TGSI_WRITEMASK_X;
   else
      writemask = TGSI_WRITEMASK_Y;

   shader = ureg_create(TGSI_PROCESSOR_FRAGMENT);
   if (!shader)
      return false;

   pos = ureg_DECL_fs_input(shader, TGSI_SEMANTIC_POSITION, 0, TGSI_INTERPOLATE_LINEAR);
   tc = ureg_DECL_fs_input(shader, TGSI_SEMANTIC_GENERIC, 1, TGSI_INTERPOLATE_LINEAR);
   fragment = ureg_DECL_output(shader, TGSI_SEMANTIC_COLOR, 0);
   sampler[0] = ureg_DECL_sampler(shader, 0);
   sampler[1] = ureg_DECL_sampler(shader, 1);
   if (!luma) {
      sampler[2] = ureg_DECL_sampler(shader, 2);
      sampler[3] = ureg_DECL_sampler(shader, 3);
   }

   nearest = luma || c->chroma != PIPE_VIDEO_CHROMA_FORMAT_420 || !interlaced;
   field = calc_line(shader, pos, nearest);
   swizcolor = ureg_DECL_temporary(shader);

   /* field.y = fraction(coord/2) < . 5
    *
    * if (field.y)
    *   swiz = sampler[top];
    * else
    *   swiz = sampler[bottom];
    *
    * if (LUMA)
    *    color.x = swiz;
    * else
    *    color.xy = swiz.yz;
    */

   ureg_IF(shader, ureg_scalar(ureg_src(field), TGSI_SWIZZLE_Y), &label);
   {
      struct ureg_dst adjtc = ureg_DECL_temporary(shader);
      if (!nearest) {
         /* -2.0 / c->video_h (1 pixel up, chroma = half height, full height wouldn't need this)
          * + .5 / c->video_h (.25 pixel down, since interlaced first pixel = .75 first
          */
         ureg_MOV(shader, ureg_writemask(adjtc, TGSI_WRITEMASK_X), tc);
         ureg_SUB(shader, ureg_writemask(adjtc, TGSI_WRITEMASK_Y), ureg_scalar(tc, TGSI_SWIZZLE_Y),
                  ureg_imm1f(shader, 1.5f / c->video_h));
      } else
         ureg_MOV(shader, ureg_writemask(adjtc, TGSI_WRITEMASK_XY), tc);
      ureg_TEX(shader, ureg_writemask(swizcolor, writemask), TGSI_TEXTURE_2D, ureg_src(adjtc), sampler[1]);
      if (!luma)
         ureg_TEX(shader, ureg_writemask(swizcolor, TGSI_WRITEMASK_Z), TGSI_TEXTURE_2D, ureg_src(adjtc), sampler[3]);
      ureg_release_temporary(shader, adjtc);
      ureg_fixup_label(shader, label, ureg_get_instruction_number(shader));
   }
   ureg_ELSE(shader, &label);
   {
      struct ureg_dst adjtc = ureg_DECL_temporary(shader);
      if (!nearest) {
         ureg_MOV(shader, ureg_writemask(adjtc, TGSI_WRITEMASK_X), tc);
         ureg_ADD(shader, ureg_writemask(adjtc, TGSI_WRITEMASK_Y), ureg_scalar(tc, TGSI_SWIZZLE_Y),
                  ureg_imm1f(shader, .5f / c->video_h));
      } else
         ureg_MOV(shader, ureg_writemask(adjtc, TGSI_WRITEMASK_XY), tc);
      ureg_TEX(shader, ureg_writemask(swizcolor, writemask), TGSI_TEXTURE_2D, ureg_src(adjtc), sampler[0]);
      if (!luma)
         ureg_TEX(shader, ureg_writemask(swizcolor, TGSI_WRITEMASK_Z), TGSI_TEXTURE_2D, ureg_src(adjtc), sampler[2]);
      ureg_release_temporary(shader, adjtc);
      ureg_fixup_label(shader, label, ureg_get_instruction_number(shader));
   }
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
create_frag_shader_bicubic(struct vl_compositor *c) {
   struct ureg_src sampler[3], lookup, cst, size, tc, csc[4];
   struct ureg_dst fragment, tmp, hg_x, hg_y, color, coord[2][2], tex[2][2];
   int i, j;

   struct ureg_program *shader;
   shader = ureg_create(TGSI_PROCESSOR_FRAGMENT);
   if (!shader)
      return NULL;

   ureg_property_fs_coord_pixel_center(shader, TGSI_FS_COORD_PIXEL_CENTER_INTEGER);
   tc = ureg_DECL_fs_input(shader, TGSI_SEMANTIC_GENERIC, 1, TGSI_INTERPOLATE_LINEAR);
   for (i = 0; i < 3; ++i)
      csc[i] = ureg_DECL_constant(shader, i);
   size = ureg_DECL_constant(shader, 4);
   cst = ureg_DECL_constant(shader, 5);

   fragment = ureg_DECL_output(shader, TGSI_SEMANTIC_COLOR, 0);
   for (i = 0; i < 3; ++i)
      sampler[i] = ureg_DECL_sampler(shader, i);
   lookup = ureg_DECL_sampler(shader, 3);

   tmp = ureg_DECL_temporary(shader);
   hg_x = ureg_DECL_temporary(shader);
   hg_y = ureg_DECL_temporary(shader);
   color = ureg_DECL_temporary(shader);

   for (i = 0; i < 4; ++i) {
      coord[i/2][i%2] = ureg_DECL_temporary(shader);
      tex[i/2][i%2] = ureg_DECL_temporary(shader);
   }

   for (j = 0; j < 3; ++j) {
      unsigned writemask, p = j >= 1;
      writemask = TGSI_WRITEMASK_X << j;

      if (j == 0 || (j == 1 && c->chroma != PIPE_VIDEO_CHROMA_FORMAT_444)) {
         /* tmp.xy = tc * size[p].xy
          * hg_x.xyz = tex1D(lookup, tmp.x);
          * hg_y.xyz = tex1D(lookup, tmp.y);
          */
         ureg_MUL(shader, ureg_writemask(tmp, TGSI_WRITEMASK_XY),
                  tc, ureg_swizzle(size, 2 * p, 2 * p + 1, 2 * p + 1, 2 * p + 1));
         ureg_FRC(shader, ureg_writemask(tmp, TGSI_WRITEMASK_XY), ureg_src(tmp));
         ureg_TEX(shader, ureg_writemask(hg_x, TGSI_WRITEMASK_XYZ),
                  TGSI_TEXTURE_1D, ureg_scalar(ureg_src(tmp), TGSI_SWIZZLE_X), lookup);
         ureg_TEX(shader, ureg_writemask(hg_y, TGSI_WRITEMASK_XYZ),
                  TGSI_TEXTURE_1D, ureg_scalar(ureg_src(tmp), TGSI_SWIZZLE_Y), lookup);

#define e_x(x, p) ureg_scalar(x, 2 * p)
#define e_y(x, p) ureg_scalar(x, 2 * p + 1)

         /* coord1X.x += hg_x.x * cst[p].x;
          * coord0X.x -= hg_x.y * cst[p].x;
          *
          * coord11.y += hg_y.x * cst[p].w
          * coord01.y += hg_y.x * cst[p].w
          *
          * coord10.y -= hg_y.y * cst[p].w
          * coord00.y -= hg_y.y * cst[p].w
          */

         ureg_MAD(shader, ureg_writemask(coord[1][0], TGSI_WRITEMASK_X),
                  ureg_scalar(ureg_src(hg_x), TGSI_SWIZZLE_X),
                  e_x(cst, p), tc);
         ureg_MAD(shader, ureg_writemask(coord[0][0], TGSI_WRITEMASK_X),
                  ureg_negate(ureg_scalar(ureg_src(hg_x), TGSI_SWIZZLE_Y)),
                  e_x(cst, p), tc);

         for (i = 0; i < 2; ++i)
            ureg_MOV(shader, ureg_writemask(coord[i][1], TGSI_WRITEMASK_X), ureg_src(coord[i][0]));
         ureg_MAD(shader, ureg_writemask(coord[1][1], TGSI_WRITEMASK_Y),
                  ureg_scalar(ureg_src(hg_y), TGSI_SWIZZLE_X),
                  e_y(cst, p), tc);
         ureg_MAD(shader, ureg_writemask(coord[0][1], TGSI_WRITEMASK_Y),
                  ureg_scalar(ureg_src(hg_y), TGSI_SWIZZLE_X),
                  e_y(cst, p), tc);

         ureg_MAD(shader, ureg_writemask(coord[1][0], TGSI_WRITEMASK_Y),
                  ureg_negate(ureg_scalar(ureg_src(hg_y), TGSI_SWIZZLE_Y)),
                  e_y(cst, p), tc);
         ureg_MAD(shader, ureg_writemask(coord[0][0], TGSI_WRITEMASK_Y),
                  ureg_negate(ureg_scalar(ureg_src(hg_y), TGSI_SWIZZLE_Y)),
                  e_y(cst, p), tc);

#undef e_y
#undef e_x
      }

      for (i = 0; i < 4; ++i) {
         ureg_TEX(shader, ureg_writemask(tex[i/2][i%2], writemask),
                  TGSI_TEXTURE_2D, ureg_src(coord[i/2][i%2]), sampler[j]);
      }

      for (i = 0; i < 2; ++i)
         ureg_LRP(shader, ureg_writemask(tex[i][0], writemask),
                  ureg_scalar(ureg_src(hg_y), TGSI_SWIZZLE_Z),
                  ureg_src(tex[i][0]), ureg_src(tex[i][1]));

      ureg_LRP(shader, ureg_writemask(color, writemask),
               ureg_scalar(ureg_src(hg_x), TGSI_SWIZZLE_Z),
               ureg_src(tex[0][0]), ureg_src(tex[1][0]));
   }

   for (i = 3; i >= 0; --i) {
      ureg_release_temporary(shader, tex[i/2][i%2]);
      ureg_release_temporary(shader, coord[i/2][i%2]);
   }
   ureg_release_temporary(shader, hg_y);
   ureg_release_temporary(shader, hg_x);
   ureg_release_temporary(shader, tmp);

   ureg_MOV(shader, ureg_writemask(color, TGSI_WRITEMASK_W), ureg_imm1f(shader, 1.0f));

   if (DEBUG_BICUBIC) {
      struct ureg_dst lincolor = ureg_DECL_temporary(shader);
      unsigned label;
      float val = ((float)DEBUG_BICUBIC) / 512.f;
      ureg_TEX(shader, ureg_writemask(lincolor, TGSI_WRITEMASK_X), TGSI_TEXTURE_2D, tc, sampler[0]);
      ureg_TEX(shader, ureg_writemask(lincolor, TGSI_WRITEMASK_Y),
               TGSI_TEXTURE_2D, tc, sampler[1]);
      ureg_TEX(shader, ureg_writemask(lincolor, TGSI_WRITEMASK_Z),
               TGSI_TEXTURE_2D, tc, sampler[2]);
      /* lincolor.xyz = tex2D(...);
       * lincolor.xyz = |color - lincolor|
       * lincolor.xyz = lincolor >= DEBUG_BICUBIC / 512.f
       * if (lincolor.xyz)
       *    color.xyz = { 1.f, .5f, .5f }
       * endif
       */
      ureg_SUB(shader, ureg_writemask(lincolor, TGSI_WRITEMASK_XYZ), ureg_src(color), ureg_src(lincolor));
      ureg_ABS(shader, ureg_writemask(lincolor, TGSI_WRITEMASK_XYZ), ureg_src(lincolor));
      ureg_SGE(shader, ureg_writemask(lincolor, TGSI_WRITEMASK_XYZ), ureg_src(lincolor), ureg_imm3f(shader, val, val, val));
      ureg_IF(shader, ureg_swizzle(ureg_src(lincolor), TGSI_SWIZZLE_X, TGSI_SWIZZLE_Y, TGSI_SWIZZLE_W, TGSI_SWIZZLE_Z), &label);
         ureg_MOV(shader, ureg_writemask(color, TGSI_WRITEMASK_XYZ), ureg_imm3f(shader, 1.f, .5f, .5f));
         ureg_fixup_label(shader, label, ureg_get_instruction_number(shader));
      ureg_ENDIF(shader);
      ureg_release_temporary(shader, lincolor);
   }

   for (i = 0; i < 3; ++i)
      ureg_DP4(shader, ureg_writemask(fragment, TGSI_WRITEMASK_X << i), csc[i], ureg_src(color));
   ureg_MOV(shader, ureg_writemask(fragment, TGSI_WRITEMASK_W), ureg_imm1f(shader, 1.0f));

   ureg_release_temporary(shader, color);

   ureg_END(shader);
   return ureg_create_shader_and_destroy(shader, c->pipe);
}

static void *
create_frag_shader_sobel(struct vl_compositor *c, unsigned planes)
{
   struct ureg_program *shader;
   struct ureg_src tc, sampler[3];
   struct ureg_dst fragment, color[4], tctmp, ly, lx;
   float v[2] = { -.5f, .5f }, wmul = 1.f, hmul = 1.f;
   int i;
   unsigned label, writemask = TGSI_WRITEMASK_X;
   if (planes > 1) {
      if (c->chroma != PIPE_VIDEO_CHROMA_FORMAT_444)
         wmul = 2.f;
      if (c->chroma == PIPE_VIDEO_CHROMA_FORMAT_420)
         hmul = 2.f;
      writemask = TGSI_WRITEMASK_YZ;
   }

   shader = ureg_create(TGSI_PROCESSOR_FRAGMENT);
   if (!shader)
      return NULL;

   tc = ureg_DECL_fs_input(shader, TGSI_SEMANTIC_GENERIC, 1, TGSI_INTERPOLATE_LINEAR);
   fragment = ureg_DECL_output(shader, TGSI_SEMANTIC_COLOR, 0);
   tctmp = ureg_DECL_temporary(shader);

   sampler[0] = ureg_DECL_sampler(shader, 0);
   if (planes) {
      for (i = 1; i < planes; ++i)
         sampler[i] = ureg_DECL_sampler(shader, i);

      ureg_TEX(shader, ureg_writemask(tctmp, TGSI_WRITEMASK_X), TGSI_TEXTURE_2D, tc, sampler[0]);
      ureg_SGE(shader, ureg_writemask(tctmp, TGSI_WRITEMASK_X), ureg_src(tctmp), ureg_imm1f(shader, vl_contour_luma_threshold));
      ureg_IF(shader, ureg_scalar(ureg_src(tctmp), TGSI_SWIZZLE_X), &label);
   }

   lx = ureg_DECL_temporary(shader);
   ly = ureg_DECL_temporary(shader);
   for (i = 0; i < Elements(color); ++i)
      color[i] = ureg_DECL_temporary(shader);

   /* Sobel, calculate textures for Ly, each point is sampling 4 others */
   /* Weighting matrix in y direction (roughly):
    *
    *  0 .125 0.5 .125  0
    *  0 .500 2.0 .500  0
    * 4 divided by 2 weightings => multiply by 2
    */
   for (i = 0; i < 4; ++i) {
      ureg_ADD(shader, ureg_writemask(tctmp, TGSI_WRITEMASK_XY), tc, ureg_imm2f(shader, .5 * wmul * v[i%2]/(float)c->video_w, hmul * 2.25f * v[i/2]/(float)c->video_h));
      if (planes < 3)
         ureg_TEX(shader, ureg_writemask(color[i], writemask), TGSI_TEXTURE_2D, ureg_src(tctmp), sampler[!!planes]);
      else {
         ureg_TEX(shader, ureg_writemask(color[i], TGSI_WRITEMASK_Y), TGSI_TEXTURE_2D, ureg_src(tctmp), sampler[1]);
         ureg_TEX(shader, ureg_writemask(color[i], TGSI_WRITEMASK_Z), TGSI_TEXTURE_2D, ureg_src(tctmp), sampler[2]);
      }
   }

   /* Ly.xyz = wtop - wbottom */
   ureg_ADD(shader, ureg_writemask(color[0], writemask), ureg_src(color[0]), ureg_src(color[1]));
   ureg_ADD(shader, ureg_writemask(color[1], writemask), ureg_src(color[2]), ureg_src(color[3]));
   ureg_SUB(shader, ureg_writemask(ly, writemask), ureg_src(color[0]), ureg_src(color[1]));

   for (i = 0; i < 4; ++i) {
      ureg_ADD(shader, ureg_writemask(tctmp, TGSI_WRITEMASK_XY), tc, ureg_imm2f(shader, wmul * 2.25f * v[i/2]/(float)c->video_w, .5 * hmul * v[i%2]/(float)c->video_h));
      if (planes < 3)
         ureg_TEX(shader, ureg_writemask(color[i], writemask), TGSI_TEXTURE_2D, ureg_src(tctmp), sampler[!!planes]);
      else {
         ureg_TEX(shader, ureg_writemask(color[i], TGSI_WRITEMASK_Y), TGSI_TEXTURE_2D, ureg_src(tctmp), sampler[1]);
         ureg_TEX(shader, ureg_writemask(color[i], TGSI_WRITEMASK_Z), TGSI_TEXTURE_2D, ureg_src(tctmp), sampler[2]);
      }
   }

   ureg_ADD(shader, ureg_writemask(color[0], writemask), ureg_src(color[0]), ureg_src(color[1]));
   ureg_ADD(shader, ureg_writemask(color[1], writemask), ureg_src(color[2]), ureg_src(color[3]));
   ureg_SUB(shader, ureg_writemask(lx, writemask), ureg_src(color[0]), ureg_src(color[1]));
   /* Lx.xyz = wleft - wright */

   if (!planes) {
      /* tmp.xyz = |Lx|**2 + |Ly|**2 */
      ureg_MUL(shader, ureg_writemask(tctmp, writemask), ureg_src(lx), ureg_src(lx));
      ureg_MAD(shader, ureg_writemask(tctmp, writemask), ureg_src(ly), ureg_src(ly), ureg_src(tctmp));
      ureg_MUL(shader, ureg_writemask(fragment, writemask), ureg_src(tctmp), ureg_imm1f(shader, 2.f));
   } else {
      static const float s = 2.f;
      ureg_ABS(shader, ureg_writemask(lx, writemask), ureg_src(lx));
      ureg_ABS(shader, ureg_writemask(ly, writemask), ureg_src(ly));
      ureg_MUL(shader, ureg_writemask(fragment, TGSI_WRITEMASK_XZ),
               ureg_swizzle(ureg_src(lx), TGSI_SWIZZLE_Y, TGSI_SWIZZLE_Y, TGSI_SWIZZLE_Z, TGSI_SWIZZLE_Z),
               ureg_imm4f(shader, s, s, s, s));
      ureg_MUL(shader, ureg_writemask(fragment, TGSI_WRITEMASK_YW),
               ureg_swizzle(ureg_src(ly), TGSI_SWIZZLE_Y, TGSI_SWIZZLE_Y, TGSI_SWIZZLE_Z, TGSI_SWIZZLE_Z),
               ureg_imm4f(shader, s, s, s, s));
   }

   for (i = 0; i < Elements(color); ++i)
      ureg_release_temporary(shader, color[i]);
   ureg_release_temporary(shader, ly);
   ureg_release_temporary(shader, lx);

   if (planes) {
      ureg_fixup_label(shader, label, ureg_get_instruction_number(shader));
      ureg_ELSE(shader, &label);
      ureg_MOV(shader, fragment, ureg_imm4f(shader, 0.f, 0.f, 0.f, 0.f));
      ureg_fixup_label(shader, label, ureg_get_instruction_number(shader));
      ureg_ENDIF(shader);
   }
   ureg_release_temporary(shader, tctmp);
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

   sampler.wrap_s = sampler.wrap_t = sampler.wrap_r = PIPE_TEX_WRAP_REPEAT;
   sampler.min_img_filter = PIPE_TEX_FILTER_LINEAR;
   sampler.mag_img_filter = PIPE_TEX_FILTER_LINEAR;
   c->sampler_repeat = c->pipe->create_sampler_state(c->pipe, &sampler);

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
   c->pipe->delete_sampler_state(c->pipe, c->sampler_repeat);
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
      sizeof(csc_matrix) + sizeof(c->original_sizes)
   );
   assert((Elements(c->csc) + Elements(c->original_sizes))/4 == 7); // amount of constants

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
         unsigned num_sampler_views = !samplers[1] ? 1 : !samplers[2] ? 2 : !samplers[3] ? 3 : !samplers[4] ? 4 : 5;

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
   for (i = 0; i < Elements(c->video_res); ++i) {
      pipe_sampler_view_reference(&c->video_sv[i], NULL);
      pipe_surface_reference(&c->video_surf[i], NULL);
      pipe_resource_reference(&c->video_res[i], NULL);
   }
   for (i = 0; i < Elements(c->fs_weave); ++i) {
      if (!c->fs_weave[i])
         continue;
      c->pipe->delete_fs_state(c->pipe, c->fs_weave[i]);
      c->fs_weave[i] = NULL;
   }

   for (i = 0; i < Elements(c->fs_video_buffer); ++i) {
      if (c->fs_video_buffer[i])
         c->pipe->delete_fs_state(c->pipe, c->fs_video_buffer[i]);
      c->fs_video_buffer[i] = NULL;
   }
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
   float *map;

   assert(c);

   map = pipe_buffer_map(c->pipe, c->csc_matrix,
                         PIPE_TRANSFER_WRITE | PIPE_TRANSFER_DISCARD,
                         &buf_transfer);
   memcpy(map, matrix, sizeof(csc_matrix));
   memcpy(map + Elements(c->csc), c->original_sizes, sizeof(c->original_sizes));
   memcpy(c->csc, matrix, sizeof(csc_matrix));
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
vl_compositor_render_sobel(struct vl_compositor *c, struct pipe_sampler_view **sv)
{
   struct pipe_scissor_state scissor;
   struct pipe_surface *dst_surface;
   void *samplers[3];
   struct pipe_sampler_view *sv_render[3];
   int i;
   for (i = 0; i < 3; ++i) {
      sv_render[i] = sv[i];
      samplers[i] = c->sampler_linear;
   }

   assert(c);

   gen_vertex_data_video(c);

   c->pipe->bind_vs_state(c->pipe, c->vs);
   c->pipe->set_vertex_buffers(c->pipe, 1, &c->vertex_buf);
   c->pipe->bind_vertex_elements_state(c->pipe, c->vertex_elems_state);
   c->pipe->bind_rasterizer_state(c->pipe, c->rast);
   c->pipe->bind_blend_state(c->pipe, c->blend_clear);
   c->pipe->bind_fragment_sampler_states(c->pipe, 3, samplers);

   for (i = 0; i < 2; ++i) {
      if (i)
         sv_render[0] = c->video_sv[3];
      c->pipe->set_fragment_sampler_views(c->pipe, 3, sv_render);

      c->viewport.scale[0] = sv[i]->texture->width0;
      c->viewport.scale[1] = sv[i]->texture->height0;
      c->viewport.translate[0] = 0;
      c->viewport.translate[1] = 0;
      c->pipe->set_viewport_state(c->pipe, &c->viewport);

      c->pipe->bind_fs_state(c->pipe, c->fs_weave[3+i]);
      dst_surface = c->video_surf[3 + i];

      c->fb_state.width = dst_surface->width;
      c->fb_state.height = dst_surface->height;
      c->fb_state.cbufs[0] = dst_surface;

      scissor.minx = 0;
      scissor.miny = 0;
      scissor.maxx = dst_surface->width;
      scissor.maxy = dst_surface->height;

      c->pipe->set_scissor_state(c->pipe, &scissor);
      c->pipe->set_framebuffer_state(c->pipe, &c->fb_state);
      util_draw_arrays(c->pipe, PIPE_PRIM_TRIANGLES, 0, 3);
   }
}

static void
vl_compositor_render_video(struct vl_compositor *c,
                           struct pipe_sampler_view **sv,
                           unsigned interlaced)
{
   struct pipe_scissor_state scissor;
   void *samplers[4];
   unsigned i;
   for (i = 0; i < 4; ++i) {
      if (!interlaced || i < 2 || c->chroma != PIPE_VIDEO_CHROMA_FORMAT_420)
         samplers[i] = c->sampler_nearest;
      else
         samplers[i] = c->sampler_linear;
   }
   assert(c);
   gen_vertex_data_video(c);
   for (i = 0; i < 2; ++i) {
      struct pipe_surface *dst_surface;
      unsigned num_sampler_views;
      void *fs;
      if (!i) {
         num_sampler_views = 2;
         dst_surface = c->video_surf[0];
         fs = c->fs_weave[0];
      } else {
         num_sampler_views = 4;
         if (interlaced) {
            dst_surface = c->video_surf[1];
            fs = c->fs_weave[1];
         } else {
            dst_surface = c->video_surf[2];
            fs = c->fs_weave[2];
         }
      }

      assert(dst_surface);
      c->fb_state.width = dst_surface->width;
      c->fb_state.height = dst_surface->height;
      c->fb_state.cbufs[0] = dst_surface;

      c->viewport.scale[0] = sv[0]->texture->width0;
      c->viewport.scale[1] = sv[0]->texture->height0 * 2;
      if (i && c->chroma == PIPE_VIDEO_CHROMA_FORMAT_420 && interlaced)
         c->viewport.scale[1] *= 2;
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
      c->pipe->bind_fs_state(c->pipe, fs);
      c->pipe->bind_fragment_sampler_states(c->pipe, num_sampler_views, samplers);
      c->pipe->set_fragment_sampler_views(c->pipe, num_sampler_views, sv);
      sv += num_sampler_views;
      util_draw_arrays(c->pipe, PIPE_PRIM_TRIANGLES, 0, 3);
   }
}

void
vl_compositor_set_buffer_layer(struct vl_compositor *c, unsigned layer,
                               enum pipe_video_picture_structure field,
                               struct pipe_video_buffer *buffer,
                               struct pipe_video_rect *src_rect,
                               struct pipe_video_rect *dst_rect,
                               unsigned past_count,
                               struct pipe_video_buffer **past,
                               unsigned future_count,
                               struct pipe_video_buffer **future)
{
   struct pipe_sampler_view **sampler_views, *sv_weave[6], *sv[VL_COMPOSITOR_SAMPLERS] = {};
   struct pipe_video_rect rect;
   unsigned i, half_h = 0;

   assert(c && buffer);
   assert(c->video_w <= buffer->width && c->video_h <= buffer->height);
   assert(layer < VL_COMPOSITOR_MAX_LAYERS);

   if (field == PIPE_VIDEO_PICTURE_STRUCTURE_FRAME) {
      sampler_views = buffer->get_sampler_view_planes(buffer, 0);
      if (!sampler_views) {
         sampler_views = buffer->get_sampler_view_planes(buffer, 1);
         for (i = 0; i < 6; ++i)
            sv_weave[i] = sampler_views[i];
         if (!sv_weave[4])
            sv_weave[4] = sv_weave[2];
         if (!sv_weave[5])
            sv_weave[5] = sv_weave[3];
         vl_compositor_render_video(c, sv_weave, 0);
         sv[0] = c->video_sv[0];
         sv[2] = sv[1] = c->video_sv[2];
      } else {
         for (i = 0; i < 3; ++i)
            sv[i] = sampler_views[i];
         if (!sv[2])
            sv[2] = sv[1];
      }
   } else {
      struct pipe_sampler_view **sv_cur, **sv_prev = NULL;
      int top = field == PIPE_VIDEO_PICTURE_STRUCTURE_FIELD_TOP;
      sv_cur = buffer->get_sampler_view_planes(buffer, 1);
      if (past_count && past[0])
         sv_prev = buffer->get_sampler_view_planes(past[0], 1);
      if (sv_prev) {
         for (i = 0; i < 6; i += 2) {
            if (top) {
               sv_weave[i] = sv_cur[i];
               sv_weave[i+1] = sv_prev[i+1];
            } else {
               sv_weave[i] = sv_prev[i];
               sv_weave[i+1] = sv_cur[i+1];
            }
         }
         if (!sv_weave[4])
            sv_weave[4] = sv_weave[2];
         if (!sv_weave[5])
            sv_weave[5] = sv_weave[3];
         vl_compositor_render_video(c, sv_weave, 1);
         sv[0] = c->video_sv[0];
         sv[2] = sv[1] = c->video_sv[1];
      } else {
         for (i = 0; i < 3; ++i)
            sv[i] = sv_cur[2*i+!top];
         if (!sv[2])
            sv[2] = sv[1];
         half_h = 1;
      }
   }
   assert(sv[2] && sv[1] && sv[0]);

   if (DEBUG_CONTOUR && !half_h)
      vl_compositor_render_sobel(c, sv);

   c->used_layers |= 1 << layer;
   if (!src_rect) {
      src_rect = &rect;
      rect.x = rect.y = 0;
      rect.w = c->video_w;
      rect.h = c->video_h;
   }
   if (DEBUG_CONTOUR && !half_h) {
      c->layers[layer].fs = c->fs_video_buffer[1];
      sv[3] = c->video_sv[3];
      sv[4] = c->video_sv[4];
   } else
      c->layers[layer].fs = c->fs_video_buffer[0];

   for (i = 0; i < VL_COMPOSITOR_SAMPLERS; ++i) {
      c->layers[layer].samplers[i] = c->sampler_linear;
      pipe_sampler_view_reference(&c->layers[layer].sampler_views[i], sv[i]);
   }
   if (USE_BICUBIC) {
      pipe_sampler_view_reference(&c->layers[layer].sampler_views[3], c->video_sv[4]);
      c->layers[layer].samplers[3] = c->sampler_repeat;
   }

   if (c->original_sizes[0] != (float)sv[0]->texture->width0 ||
       c->original_sizes[1] != (float)sv[0]->texture->height0 ||
       c->original_sizes[2] != (float)sv[1]->texture->width0 ||
       c->original_sizes[3] != (float)sv[1]->texture->height0) {
      struct pipe_transfer *buf_transfer;
      float *map = pipe_buffer_map(c->pipe, c->csc_matrix,
                                   PIPE_TRANSFER_WRITE | PIPE_TRANSFER_DISCARD,
                                   &buf_transfer);
      c->original_sizes[0] = (float)sv[0]->texture->width0;
      c->original_sizes[1] = (float)sv[0]->texture->height0;
      c->original_sizes[2] = (float)sv[1]->texture->width0;
      c->original_sizes[3] = (float)sv[1]->texture->height0;
      c->original_sizes[4] = 1.f/(float)sv[0]->texture->width0;
      c->original_sizes[5] = 1.f/(float)sv[0]->texture->height0;
      c->original_sizes[6] = 1.f/(float)sv[1]->texture->width0;
      c->original_sizes[7] = 1.f/(float)sv[1]->texture->height0;
      c->original_sizes[8] = .5f/(float)sv[0]->texture->width0;
      c->original_sizes[9] = .5f/(float)sv[0]->texture->height0;
      c->original_sizes[10] = .5f/(float)sv[1]->texture->width0;
      c->original_sizes[11] = .5f/(float)sv[1]->texture->height0;
      memcpy(map, c->csc, sizeof(c->csc));
      memcpy(map + Elements(c->csc), c->original_sizes, sizeof(c->original_sizes));
      pipe_buffer_unmap(c->pipe, buf_transfer);
   }

   calc_src_and_dst(&c->layers[layer], sv[0]->texture->width0,
                    sv[0]->texture->height0 << half_h, *src_rect,
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
   int i;

   assert(layer < VL_COMPOSITOR_MAX_LAYERS);

   c->used_layers |= 1 << layer;

   c->layers[layer].fs = include_color_conversion ?
      c->fs_palette.yuv : c->fs_palette.rgb;

   c->layers[layer].samplers[0] = c->sampler_linear;
   c->layers[layer].samplers[1] = c->sampler_nearest;
   pipe_sampler_view_reference(&c->layers[layer].sampler_views[0], indexes);
   pipe_sampler_view_reference(&c->layers[layer].sampler_views[1], palette);
   for (i = 2; i < VL_COMPOSITOR_SAMPLERS; ++i) {
      pipe_sampler_view_reference(&c->layers[layer].sampler_views[i], NULL);
      c->layers[layer].samplers[i] = NULL;
   }
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
   int i;

   assert(layer < VL_COMPOSITOR_MAX_LAYERS);

   c->used_layers |= 1 << layer;
   c->layers[layer].fs = c->fs_rgba;
   c->layers[layer].samplers[0] = c->sampler_linear;
   pipe_sampler_view_reference(&c->layers[layer].sampler_views[0], rgba);
   for (i = 1; i < VL_COMPOSITOR_SAMPLERS; ++i) {
      pipe_sampler_view_reference(&c->layers[layer].sampler_views[i], NULL);
      c->layers[layer].samplers[i] = NULL;
   }
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
   vl_compositor_set_csc_matrix(c, c->csc);

   c->clear_color.f[0] = c->clear_color.f[1] = 0.0f;
   c->clear_color.f[2] = c->clear_color.f[3] = 0.0f;

   return true;
}

bool
vl_compositor_init_video(struct vl_compositor *c, struct pipe_context *pipe,
                         enum pipe_video_chroma_format chroma, unsigned w, unsigned h)
{
   struct pipe_resource templ;
   int i;
   if (!vl_compositor_init(c, pipe))
      return false;
   c->video_w = w;
   c->video_h = h;

   if (USE_BICUBIC)
      c->fs_video_buffer[0] = create_frag_shader_bicubic(c);
   else
      c->fs_video_buffer[0] = create_frag_shader_video_buffer(c);
   if (DEBUG_CONTOUR)
      c->fs_video_buffer[1] = create_frag_shader_sobel_video(c);

   for (i = 0; i < Elements(c->fs_video_buffer); ++i) {
      if (i == 1 && !DEBUG_CONTOUR) continue;
      if (!c->fs_video_buffer[i]) {
         debug_printf("Unable to create YCbCr-to-RGB fragment shader %i.\n", i);
         goto fail;
      }
   }
   c->fs_weave[0] = create_frag_shader_weave(c, 1, 0);
   c->fs_weave[1] = create_frag_shader_weave(c, 0, 1); // interlaced
   c->fs_weave[2] = create_frag_shader_weave(c, 0, 0); // progressive
   if (DEBUG_CONTOUR) {
      c->fs_weave[3] = create_frag_shader_sobel(c, 0);
      c->fs_weave[4] = create_frag_shader_sobel(c, 3);
   }
   for (i = 0; i < Elements(c->fs_weave); ++i) {
      if (!DEBUG_CONTOUR && (i >= 3 && i <= 4)) continue;
      if (!c->fs_weave[i]) {
         debug_printf("Unable to create weave fragment shaders [%i].\n", i);
         goto fail;
      }
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
   c->video_res[0] = pipe->screen->resource_create(pipe->screen, &templ);
   if (!c->video_res[0]) {
      debug_printf("Could not create weave temp frame for luma\n");
      goto fail;
   }

   if (chroma != PIPE_VIDEO_CHROMA_FORMAT_444)
      templ.width0 /= 2;
   templ.format = PIPE_FORMAT_R8G8_UNORM;
   c->video_res[1] = pipe->screen->resource_create(pipe->screen, &templ);
   if (!c->video_res[1]) {
      debug_printf("Could not create interlaced temp frame for chroma\n");
      goto fail;
   }

   if (chroma == PIPE_VIDEO_CHROMA_FORMAT_420)
      templ.height0 = h / 2;
   c->video_res[2] = pipe->screen->resource_create(pipe->screen, &templ);
   if (!c->video_res[2]) {
      debug_printf("Could not create deinterlaced temp frame for chroma\n");
      goto fail;
   }

   if (DEBUG_CONTOUR) {
      c->video_res[3] = pipe->screen->resource_create(pipe->screen, c->video_res[0]);
      if (!c->video_res[3]) {
         debug_printf("Could not create sobel temp frame for luma\n");
         goto fail;
      }
      templ.format = PIPE_FORMAT_R8G8B8A8_UNORM;
      c->video_res[4] = pipe->screen->resource_create(pipe->screen, &templ);
      if (!c->video_res[4]) {
         debug_printf("Could not create sobel temp frame for chroma\n");
         goto fail;
      }
   }

   if (USE_BICUBIC) {
      memset(&templ, 0, sizeof(templ));
      templ.target = PIPE_TEXTURE_1D;
      templ.format = PIPE_FORMAT_R16G16B16A16_UNORM;
      templ.width0 = 256;
      templ.height0 = 1;
      templ.depth0 = 1;
      templ.array_size = 1;
      templ.bind = PIPE_BIND_SAMPLER_VIEW;
      templ.usage = PIPE_USAGE_STATIC;
      c->video_res[5] = pipe->screen->resource_create(pipe->screen, &templ);
      if (!c->video_res[5]) {
         debug_printf("Could not generate lookup texture\n");
         goto fail;
      }
   }
   if (c->video_res[5]) {
      struct pipe_transfer *buf_transfer;
      unsigned short *map = pipe_buffer_map(c->pipe, c->video_res[4],
                                            PIPE_TRANSFER_WRITE | PIPE_TRANSFER_DISCARD,
                                            &buf_transfer);

      for (i = 0; i < templ.width0; ++i, map += 4) {
         float weight[4], h0, h1, g0;
         float alpha = (float)i / (float)templ.width0;
         float alpha2 = alpha * alpha;
         float alpha3 = alpha2 * alpha;
         float mul = (float)((sizeof(*map)<<8)-1);

         weight[0] = (-alpha3 + 3.f * alpha2 - 3.f * alpha + 1.f) / 6.f;
         weight[1] = (3.f * alpha3 - 6.f * alpha2 + 4.f) / 6.f;
         weight[2] = (-3.f * alpha3 + 3.f * alpha2 + 3.f * alpha + 1.f) / 6.f;
         weight[3] = alpha3 / 6.f;
         h0 = 1.f + alpha - weight[1] / (weight[0] + weight[1]);
         h1 = 1.f - alpha + weight[3] / (weight[2] + weight[3]);
         g0 = weight[0] + weight[1];
         map[0] = h0 * mul;
         map[1] = h1 * mul;
         map[2] = g0 * mul;
         map[3] = 0;
      }
      pipe_buffer_unmap(c->pipe, buf_transfer);
   }

   for (i = 0; i < Elements(c->video_res); ++i) {
      struct pipe_sampler_view sv_templ;
      struct pipe_surface surf_templ;
      if (!c->video_res[i]) continue;

      memset(&sv_templ, 0, sizeof(sv_templ));
      u_sampler_view_default_template(&sv_templ, c->video_res[i], c->video_res[i]->format);
      if (c->video_res[i]->format == PIPE_FORMAT_R8_UNORM)
         sv_templ.swizzle_a = sv_templ.swizzle_b = sv_templ.swizzle_g = sv_templ.swizzle_r;
      else if (c->video_res[i]->format == PIPE_FORMAT_R8G8_UNORM) {
         sv_templ.swizzle_b = PIPE_SWIZZLE_GREEN;
         sv_templ.swizzle_g = PIPE_SWIZZLE_RED;
      }
      c->video_sv[i] = pipe->create_sampler_view(pipe, c->video_res[i], &sv_templ);

      if (!c->video_sv[i]) {
         debug_printf("Could not create temp video sampler views\n");
         goto fail;
      }

      memset(&surf_templ, 0, sizeof(surf_templ));
      surf_templ.usage = PIPE_BIND_SAMPLER_VIEW | PIPE_BIND_RENDER_TARGET;
      surf_templ.format = c->video_res[i]->format;
      c->video_surf[i] = pipe->create_surface(pipe, c->video_res[i], &surf_templ);
      if (!c->video_surf[i]) {
         debug_printf("Could not create temp video surface\n");
         goto fail;
      }
   }

   return true;

fail:
   vl_compositor_cleanup(c);
   return false;
}
