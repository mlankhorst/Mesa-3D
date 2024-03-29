/*
 * Copyright © 2009 Intel Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 * Authors:
 *    Eric Anholt <eric@anholt.net>
 *
 */

#include "brw_context.h"
#include "brw_state.h"
#include "brw_defines.h"
#include "brw_util.h"
#include "intel_batchbuffer.h"

/**
 * Return true if at least one of the inputs used by the given fragment
 * program has the GLSL "noperspective" interpolation qualifier.
 */
bool
brw_fprog_uses_noperspective(const struct gl_fragment_program *fprog)
{
   int attr;
   for (attr = 0; attr < FRAG_ATTRIB_MAX; ++attr) {
      /* Ignore unused inputs. */
      if (!(fprog->Base.InputsRead & BITFIELD64_BIT(attr)))
         continue;

      if (fprog->InterpQualifier[attr] == INTERP_QUALIFIER_NOPERSPECTIVE)
         return true;
   }
   return false;
}


static void
upload_clip_state(struct brw_context *brw)
{
   struct intel_context *intel = &brw->intel;
   struct gl_context *ctx = &intel->ctx;
   uint32_t depth_clamp = 0;
   uint32_t provoking, userclip;
   uint32_t nonperspective_barycentric_enable_flag = 0;
   /* BRW_NEW_FRAGMENT_PROGRAM */
   const struct gl_fragment_program *fprog = brw->fragment_program;

   if (brw_fprog_uses_noperspective(fprog)) {
      nonperspective_barycentric_enable_flag =
         GEN6_CLIP_NON_PERSPECTIVE_BARYCENTRIC_ENABLE;
   }

   if (brw->hiz.op) {
      /* HiZ operations emit a rectangle primitive, which requires clipping to
       * be disabled. From page 10 of the Sandy Bridge PRM Volume 2 Part 1
       * Section 1.3 3D Primitives Overview:
       *    RECTLIST:
       *    Either the CLIP unit should be DISABLED, or the CLIP unit's Clip
       *    Mode should be set to a value other than CLIPMODE_NORMAL.
       */
      BEGIN_BATCH(4);
      OUT_BATCH(_3DSTATE_CLIP << 16 | (4 - 2));
      OUT_BATCH(0);
      OUT_BATCH(0);
      OUT_BATCH(0);
      ADVANCE_BATCH();
      return;
   }

   if (!ctx->Transform.DepthClamp)
      depth_clamp = GEN6_CLIP_Z_TEST;

   /* _NEW_LIGHT */
   if (ctx->Light.ProvokingVertex == GL_FIRST_VERTEX_CONVENTION) {
      provoking =
	 (0 << GEN6_CLIP_TRI_PROVOKE_SHIFT) |
	 (1 << GEN6_CLIP_TRIFAN_PROVOKE_SHIFT) |
	 (0 << GEN6_CLIP_LINE_PROVOKE_SHIFT);
   } else {
      provoking =
	 (2 << GEN6_CLIP_TRI_PROVOKE_SHIFT) |
	 (2 << GEN6_CLIP_TRIFAN_PROVOKE_SHIFT) |
	 (1 << GEN6_CLIP_LINE_PROVOKE_SHIFT);
   }

   /* _NEW_TRANSFORM */
   userclip = ctx->Transform.ClipPlanesEnabled;

   BEGIN_BATCH(4);
   OUT_BATCH(_3DSTATE_CLIP << 16 | (4 - 2));
   OUT_BATCH(GEN6_CLIP_STATISTICS_ENABLE);
   OUT_BATCH(GEN6_CLIP_ENABLE |
	     GEN6_CLIP_API_OGL |
	     GEN6_CLIP_MODE_NORMAL |
             nonperspective_barycentric_enable_flag |
	     GEN6_CLIP_XY_TEST |
	     userclip << GEN6_USER_CLIP_CLIP_DISTANCES_SHIFT |
	     depth_clamp |
	     provoking);
   OUT_BATCH(U_FIXED(0.125, 3) << GEN6_CLIP_MIN_POINT_WIDTH_SHIFT |
             U_FIXED(255.875, 3) << GEN6_CLIP_MAX_POINT_WIDTH_SHIFT |
             GEN6_CLIP_FORCE_ZERO_RTAINDEX);
   ADVANCE_BATCH();
}

const struct brw_tracked_state gen6_clip_state = {
   .dirty = {
      .mesa  = _NEW_TRANSFORM | _NEW_LIGHT,
      .brw   = (BRW_NEW_CONTEXT |
                BRW_NEW_FRAGMENT_PROGRAM |
                BRW_NEW_HIZ),
      .cache = 0
   },
   .emit = upload_clip_state,
};
