# Mesa 3-D graphics library
#
# Copyright (C) 2010-2011 Chia-I Wu <olvaffe@gmail.com>
# Copyright (C) 2010-2011 LunarG Inc.
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.

LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_SRC_FILES := \
	egl.c \
	egl_pipe.c \
	egl_st.c

LOCAL_CFLAGS := \
	-DFEATURE_ES1=1 \
	-DFEATURE_ES2=1 \
	-D_EGL_MAIN=_eglBuiltInDriverGALLIUM

LOCAL_C_INCLUDES := \
	$(GALLIUM_TOP)/state_trackers/vega \
	$(GALLIUM_TOP)/state_trackers/egl \
	$(MESA_TOP)/src/egl/main \
	$(MESA_TOP)/src/mesa \
	$(DRM_TOP)/include/drm \
	$(DRM_TOP)

# swrast
LOCAL_CFLAGS += -DGALLIUM_SOFTPIPE

LOCAL_MODULE := libmesa_egl_gallium

include $(GALLIUM_COMMON_MK)
include $(BUILD_STATIC_LIBRARY)