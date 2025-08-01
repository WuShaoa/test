#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; UCD property: canonical-cm-second

;;; Generated from Unicode 13.0.0

(declare (usual-integrations))

(define ucd-canonical-cm-second-keys
  #(#(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x300 #\x301 #\x302 #\x303 #\x304 #\x306 #\x307 #\x308 #\x309 #\x30a #\x30c #\x30f #\x311 #\x323 #\x325 #\x328)
    #(#\x307 #\x323 #\x331)
    #(#\x301 #\x302 #\x307 #\x30c #\x327)
    #(#\x307 #\x30c #\x323 #\x327 #\x32d #\x331)
    #(#\x300 #\x301 #\x302 #\x303 #\x304 #\x306 #\x307 #\x308 #\x309 #\x30c #\x30f #\x311 #\x323 #\x327 #\x328 #\x32d #\x330)
    #(#\x307)
    #(#\x301 #\x302 #\x304 #\x306 #\x307 #\x30c #\x327)
    #(#\x302 #\x307 #\x308 #\x30c #\x323 #\x327 #\x32e)
    #(#\x300 #\x301 #\x302 #\x303 #\x304 #\x306 #\x307 #\x308 #\x309 #\x30c #\x30f #\x311 #\x323 #\x328 #\x330)
    #(#\x302)
    #(#\x301 #\x30c #\x323 #\x327 #\x331)
    #(#\x301 #\x30c #\x323 #\x327 #\x32d #\x331)
    #(#\x301 #\x307 #\x323)
    #(#\x300 #\x301 #\x303 #\x307 #\x30c #\x323 #\x327 #\x32d #\x331)
    #(#\x300 #\x301 #\x302 #\x303 #\x304 #\x306 #\x307 #\x308 #\x309 #\x30b #\x30c #\x30f #\x311 #\x31b #\x323 #\x328)
    #(#\x301 #\x307)
    #(#\x301 #\x307 #\x30c #\x30f #\x311 #\x323 #\x327 #\x331)
    #(#\x301 #\x302 #\x307 #\x30c #\x323 #\x326 #\x327)
    #(#\x307 #\x30c #\x323 #\x326 #\x327 #\x32d #\x331)
    #(#\x300 #\x301 #\x302 #\x303 #\x304 #\x306 #\x308 #\x309 #\x30a #\x30b #\x30c #\x30f #\x311 #\x31b #\x323 #\x324 #\x328 #\x32d #\x330)
    #(#\x303 #\x323)
    #(#\x300 #\x301 #\x302 #\x307 #\x308 #\x323)
    #(#\x307 #\x308)
    #(#\x300 #\x301 #\x302 #\x303 #\x304 #\x307 #\x308 #\x309 #\x323)
    #(#\x301 #\x302 #\x307 #\x30c #\x323 #\x331)
    #(#\x300 #\x301 #\x302 #\x303 #\x304 #\x306 #\x307 #\x308 #\x309 #\x30a #\x30c #\x30f #\x311 #\x323 #\x325 #\x328)
    #(#\x307 #\x323 #\x331)
    #(#\x301 #\x302 #\x307 #\x30c #\x327)
    #(#\x307 #\x30c #\x323 #\x327 #\x32d #\x331)
    #(#\x300 #\x301 #\x302 #\x303 #\x304 #\x306 #\x307 #\x308 #\x309 #\x30c #\x30f #\x311 #\x323 #\x327 #\x328 #\x32d #\x330)
    #(#\x307)
    #(#\x301 #\x302 #\x304 #\x306 #\x307 #\x30c #\x327)
    #(#\x302 #\x307 #\x308 #\x30c #\x323 #\x327 #\x32e #\x331)
    #(#\x300 #\x301 #\x302 #\x303 #\x304 #\x306 #\x308 #\x309 #\x30c #\x30f #\x311 #\x323 #\x328 #\x330)
    #(#\x302 #\x30c)
    #(#\x301 #\x30c #\x323 #\x327 #\x331)
    #(#\x301 #\x30c #\x323 #\x327 #\x32d #\x331)
    #(#\x301 #\x307 #\x323)
    #(#\x300 #\x301 #\x303 #\x307 #\x30c #\x323 #\x327 #\x32d #\x331)
    #(#\x300 #\x301 #\x302 #\x303 #\x304 #\x306 #\x307 #\x308 #\x309 #\x30b #\x30c #\x30f #\x311 #\x31b #\x323 #\x328)
    #(#\x301 #\x307)
    #(#\x301 #\x307 #\x30c #\x30f #\x311 #\x323 #\x327 #\x331)
    #(#\x301 #\x302 #\x307 #\x30c #\x323 #\x326 #\x327)
    #(#\x307 #\x308 #\x30c #\x323 #\x326 #\x327 #\x32d #\x331)
    #(#\x300 #\x301 #\x302 #\x303 #\x304 #\x306 #\x308 #\x309 #\x30a #\x30b #\x30c #\x30f #\x311 #\x31b #\x323 #\x324 #\x328 #\x32d #\x330)
    #(#\x303 #\x323)
    #(#\x300 #\x301 #\x302 #\x307 #\x308 #\x30a #\x323)
    #(#\x307 #\x308)
    #(#\x300 #\x301 #\x302 #\x303 #\x304 #\x307 #\x308 #\x309 #\x30a #\x323)
    #(#\x301 #\x302 #\x307 #\x30c #\x323 #\x331)
    #(#\x300 #\x301 #\x342)
    #(#\x300 #\x301 #\x303 #\x309)
    #(#\x304)
    #(#\x301)
    #(#\x301 #\x304)
    #(#\x301)
    #(#\x300 #\x301 #\x303 #\x309)
    #(#\x301)
    #(#\x300 #\x301 #\x303 #\x309)
    #(#\x301 #\x304 #\x308)
    #(#\x304)
    #(#\x301)
    #(#\x300 #\x301 #\x304 #\x30c)
    #(#\x300 #\x301 #\x303 #\x309)
    #(#\x304)
    #(#\x301)
    #(#\x301 #\x304)
    #(#\x301)
    #(#\x300 #\x301 #\x303 #\x309)
    #(#\x301)
    #(#\x300 #\x301 #\x303 #\x309)
    #(#\x301 #\x304 #\x308)
    #(#\x304)
    #(#\x301)
    #(#\x300 #\x301 #\x304 #\x30c)
    #(#\x300 #\x301 #\x303 #\x309)
    #(#\x300 #\x301 #\x303 #\x309)
    #(#\x300 #\x301)
    #(#\x300 #\x301)
    #(#\x300 #\x301)
    #(#\x300 #\x301)
    #(#\x307)
    #(#\x307)
    #(#\x307)
    #(#\x307)
    #(#\x301)
    #(#\x301)
    #(#\x308)
    #(#\x308)
    #(#\x307)
    #(#\x300 #\x301 #\x303 #\x309 #\x323)
    #(#\x300 #\x301 #\x303 #\x309 #\x323)
    #(#\x300 #\x301 #\x303 #\x309 #\x323)
    #(#\x300 #\x301 #\x303 #\x309 #\x323)
    #(#\x30c)
    #(#\x304)
    #(#\x304)
    #(#\x304)
    #(#\x304)
    #(#\x306)
    #(#\x306)
    #(#\x304)
    #(#\x304)
    #(#\x30c)
    #(#\x300 #\x301 #\x304 #\x306 #\x313 #\x314 #\x345)
    #(#\x300 #\x301 #\x313 #\x314)
    #(#\x300 #\x301 #\x313 #\x314 #\x345)
    #(#\x300 #\x301 #\x304 #\x306 #\x308 #\x313 #\x314)
    #(#\x300 #\x301 #\x313 #\x314)
    #(#\x314)
    #(#\x300 #\x301 #\x304 #\x306 #\x308 #\x314)
    #(#\x300 #\x301 #\x313 #\x314 #\x345)
    #(#\x345)
    #(#\x345)
    #(#\x300 #\x301 #\x304 #\x306 #\x313 #\x314 #\x342 #\x345)
    #(#\x300 #\x301 #\x313 #\x314)
    #(#\x300 #\x301 #\x313 #\x314 #\x342 #\x345)
    #(#\x300 #\x301 #\x304 #\x306 #\x308 #\x313 #\x314 #\x342)
    #(#\x300 #\x301 #\x313 #\x314)
    #(#\x313 #\x314)
    #(#\x300 #\x301 #\x304 #\x306 #\x308 #\x313 #\x314 #\x342)
    #(#\x300 #\x301 #\x313 #\x314 #\x342 #\x345)
    #(#\x300 #\x301 #\x342)
    #(#\x300 #\x301 #\x342)
    #(#\x345)
    #(#\x301 #\x308)
    #(#\x308)
    #(#\x306 #\x308)
    #(#\x301)
    #(#\x300 #\x306 #\x308)
    #(#\x306 #\x308)
    #(#\x308)
    #(#\x300 #\x304 #\x306 #\x308)
    #(#\x301)
    #(#\x308)
    #(#\x304 #\x306 #\x308 #\x30b)
    #(#\x308)
    #(#\x308)
    #(#\x308)
    #(#\x306 #\x308)
    #(#\x301)
    #(#\x300 #\x306 #\x308)
    #(#\x306 #\x308)
    #(#\x308)
    #(#\x300 #\x304 #\x306 #\x308)
    #(#\x301)
    #(#\x308)
    #(#\x304 #\x306 #\x308 #\x30b)
    #(#\x308)
    #(#\x308)
    #(#\x308)
    #(#\x308)
    #(#\x30f)
    #(#\x30f)
    #(#\x308)
    #(#\x308)
    #(#\x308)
    #(#\x308)
    #(#\x653 #\x654 #\x655)
    #(#\x654)
    #(#\x654)
    #(#\x654)
    #(#\x654)
    #(#\x654)
    #(#\x93c)
    #(#\x93c)
    #(#\x93c)
    #(#\x9be #\x9d7)
    #(#\xb3e #\xb56 #\xb57)
    #(#\xbd7)
    #(#\xbbe #\xbd7)
    #(#\xbbe)
    #(#\xc56)
    #(#\xcd5)
    #(#\xcc2 #\xcd5 #\xcd6)
    #(#\xcd5)
    #(#\xd3e #\xd57)
    #(#\xd3e)
    #(#\xdca #\xdcf #\xddf)
    #(#\xdca)
    #(#\x102e)
    #(#\x1b35)
    #(#\x1b35)
    #(#\x1b35)
    #(#\x1b35)
    #(#\x1b35)
    #(#\x1b35)
    #(#\x1b35)
    #(#\x1b35)
    #(#\x1b35)
    #(#\x1b35)
    #(#\x1b35)
    #(#\x304)
    #(#\x304)
    #(#\x304)
    #(#\x304)
    #(#\x307)
    #(#\x307)
    #(#\x302 #\x306)
    #(#\x302 #\x306)
    #(#\x302)
    #(#\x302)
    #(#\x302)
    #(#\x302)
    #(#\x300 #\x301 #\x342 #\x345)
    #(#\x300 #\x301 #\x342 #\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x300 #\x301 #\x342 #\x345)
    #(#\x300 #\x301 #\x342 #\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x300 #\x301)
    #(#\x300 #\x301)
    #(#\x300 #\x301)
    #(#\x300 #\x301)
    #(#\x300 #\x301 #\x342 #\x345)
    #(#\x300 #\x301 #\x342 #\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x300 #\x301 #\x342 #\x345)
    #(#\x300 #\x301 #\x342 #\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x300 #\x301 #\x342)
    #(#\x300 #\x301 #\x342)
    #(#\x300 #\x301 #\x342)
    #(#\x300 #\x301 #\x342)
    #(#\x300 #\x301)
    #(#\x300 #\x301)
    #(#\x300 #\x301)
    #(#\x300 #\x301)
    #(#\x300 #\x301 #\x342)
    #(#\x300 #\x301 #\x342)
    #(#\x300 #\x301 #\x342)
    #(#\x300 #\x301 #\x342 #\x345)
    #(#\x300 #\x301 #\x342 #\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x300 #\x301 #\x342 #\x345)
    #(#\x300 #\x301 #\x342 #\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x345)
    #(#\x300 #\x301 #\x342)
    #(#\x345)
    #(#\x345)
    #(#\x300 #\x301 #\x342)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x338)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099 #\x309a)
    #(#\x3099 #\x309a)
    #(#\x3099 #\x309a)
    #(#\x3099 #\x309a)
    #(#\x3099 #\x309a)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099 #\x309a)
    #(#\x3099 #\x309a)
    #(#\x3099 #\x309a)
    #(#\x3099 #\x309a)
    #(#\x3099 #\x309a)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x3099)
    #(#\x110ba)
    #(#\x110ba)
    #(#\x110ba)
    #(#\x11127)
    #(#\x11127)
    #(#\x1133e #\x11357)
    #(#\x114b0 #\x114ba #\x114bd)
    #(#\x115af)
    #(#\x115af)
    #(#\x11930)))

(define ucd-canonical-cm-second-values
  #(#(#\x226e)
    #(#\x2260)
    #(#\x226f)
    #(#\xc0 #\xc1 #\xc2 #\xc3 #\x100 #\x102 #\x226 #\xc4 #\x1ea2 #\xc5 #\x1cd #\x200 #\x202 #\x1ea0 #\x1e00 #\x104)
    #(#\x1e02 #\x1e04 #\x1e06)
    #(#\x106 #\x108 #\x10a #\x10c #\xc7)
    #(#\x1e0a #\x10e #\x1e0c #\x1e10 #\x1e12 #\x1e0e)
    #(#\xc8 #\xc9 #\xca #\x1ebc #\x112 #\x114 #\x116 #\xcb #\x1eba #\x11a #\x204 #\x206 #\x1eb8 #\x228 #\x118 #\x1e18 #\x1e1a)
    #(#\x1e1e)
    #(#\x1f4 #\x11c #\x1e20 #\x11e #\x120 #\x1e6 #\x122)
    #(#\x124 #\x1e22 #\x1e26 #\x21e #\x1e24 #\x1e28 #\x1e2a)
    #(#\xcc #\xcd #\xce #\x128 #\x12a #\x12c #\x130 #\xcf #\x1ec8 #\x1cf #\x208 #\x20a #\x1eca #\x12e #\x1e2c)
    #(#\x134)
    #(#\x1e30 #\x1e8 #\x1e32 #\x136 #\x1e34)
    #(#\x139 #\x13d #\x1e36 #\x13b #\x1e3c #\x1e3a)
    #(#\x1e3e #\x1e40 #\x1e42)
    #(#\x1f8 #\x143 #\xd1 #\x1e44 #\x147 #\x1e46 #\x145 #\x1e4a #\x1e48)
    #(#\xd2 #\xd3 #\xd4 #\xd5 #\x14c #\x14e #\x22e #\xd6 #\x1ece #\x150 #\x1d1 #\x20c #\x20e #\x1a0 #\x1ecc #\x1ea)
    #(#\x1e54 #\x1e56)
    #(#\x154 #\x1e58 #\x158 #\x210 #\x212 #\x1e5a #\x156 #\x1e5e)
    #(#\x15a #\x15c #\x1e60 #\x160 #\x1e62 #\x218 #\x15e)
    #(#\x1e6a #\x164 #\x1e6c #\x21a #\x162 #\x1e70 #\x1e6e)
    #(#\xd9 #\xda #\xdb #\x168 #\x16a #\x16c #\xdc #\x1ee6 #\x16e #\x170 #\x1d3 #\x214 #\x216 #\x1af #\x1ee4 #\x1e72 #\x172 #\x1e76 #\x1e74)
    #(#\x1e7c #\x1e7e)
    #(#\x1e80 #\x1e82 #\x174 #\x1e86 #\x1e84 #\x1e88)
    #(#\x1e8a #\x1e8c)
    #(#\x1ef2 #\xdd #\x176 #\x1ef8 #\x232 #\x1e8e #\x178 #\x1ef6 #\x1ef4)
    #(#\x179 #\x1e90 #\x17b #\x17d #\x1e92 #\x1e94)
    #(#\xe0 #\xe1 #\xe2 #\xe3 #\x101 #\x103 #\x227 #\xe4 #\x1ea3 #\xe5 #\x1ce #\x201 #\x203 #\x1ea1 #\x1e01 #\x105)
    #(#\x1e03 #\x1e05 #\x1e07)
    #(#\x107 #\x109 #\x10b #\x10d #\xe7)
    #(#\x1e0b #\x10f #\x1e0d #\x1e11 #\x1e13 #\x1e0f)
    #(#\xe8 #\xe9 #\xea #\x1ebd #\x113 #\x115 #\x117 #\xeb #\x1ebb #\x11b #\x205 #\x207 #\x1eb9 #\x229 #\x119 #\x1e19 #\x1e1b)
    #(#\x1e1f)
    #(#\x1f5 #\x11d #\x1e21 #\x11f #\x121 #\x1e7 #\x123)
    #(#\x125 #\x1e23 #\x1e27 #\x21f #\x1e25 #\x1e29 #\x1e2b #\x1e96)
    #(#\xec #\xed #\xee #\x129 #\x12b #\x12d #\xef #\x1ec9 #\x1d0 #\x209 #\x20b #\x1ecb #\x12f #\x1e2d)
    #(#\x135 #\x1f0)
    #(#\x1e31 #\x1e9 #\x1e33 #\x137 #\x1e35)
    #(#\x13a #\x13e #\x1e37 #\x13c #\x1e3d #\x1e3b)
    #(#\x1e3f #\x1e41 #\x1e43)
    #(#\x1f9 #\x144 #\xf1 #\x1e45 #\x148 #\x1e47 #\x146 #\x1e4b #\x1e49)
    #(#\xf2 #\xf3 #\xf4 #\xf5 #\x14d #\x14f #\x22f #\xf6 #\x1ecf #\x151 #\x1d2 #\x20d #\x20f #\x1a1 #\x1ecd #\x1eb)
    #(#\x1e55 #\x1e57)
    #(#\x155 #\x1e59 #\x159 #\x211 #\x213 #\x1e5b #\x157 #\x1e5f)
    #(#\x15b #\x15d #\x1e61 #\x161 #\x1e63 #\x219 #\x15f)
    #(#\x1e6b #\x1e97 #\x165 #\x1e6d #\x21b #\x163 #\x1e71 #\x1e6f)
    #(#\xf9 #\xfa #\xfb #\x169 #\x16b #\x16d #\xfc #\x1ee7 #\x16f #\x171 #\x1d4 #\x215 #\x217 #\x1b0 #\x1ee5 #\x1e73 #\x173 #\x1e77 #\x1e75)
    #(#\x1e7d #\x1e7f)
    #(#\x1e81 #\x1e83 #\x175 #\x1e87 #\x1e85 #\x1e98 #\x1e89)
    #(#\x1e8b #\x1e8d)
    #(#\x1ef3 #\xfd #\x177 #\x1ef9 #\x233 #\x1e8f #\xff #\x1ef7 #\x1e99 #\x1ef5)
    #(#\x17a #\x1e91 #\x17c #\x17e #\x1e93 #\x1e95)
    #(#\x1fed #\x385 #\x1fc1)
    #(#\x1ea6 #\x1ea4 #\x1eaa #\x1ea8)
    #(#\x1de)
    #(#\x1fa)
    #(#\x1fc #\x1e2)
    #(#\x1e08)
    #(#\x1ec0 #\x1ebe #\x1ec4 #\x1ec2)
    #(#\x1e2e)
    #(#\x1ed2 #\x1ed0 #\x1ed6 #\x1ed4)
    #(#\x1e4c #\x22c #\x1e4e)
    #(#\x22a)
    #(#\x1fe)
    #(#\x1db #\x1d7 #\x1d5 #\x1d9)
    #(#\x1ea7 #\x1ea5 #\x1eab #\x1ea9)
    #(#\x1df)
    #(#\x1fb)
    #(#\x1fd #\x1e3)
    #(#\x1e09)
    #(#\x1ec1 #\x1ebf #\x1ec5 #\x1ec3)
    #(#\x1e2f)
    #(#\x1ed3 #\x1ed1 #\x1ed7 #\x1ed5)
    #(#\x1e4d #\x22d #\x1e4f)
    #(#\x22b)
    #(#\x1ff)
    #(#\x1dc #\x1d8 #\x1d6 #\x1da)
    #(#\x1eb0 #\x1eae #\x1eb4 #\x1eb2)
    #(#\x1eb1 #\x1eaf #\x1eb5 #\x1eb3)
    #(#\x1e14 #\x1e16)
    #(#\x1e15 #\x1e17)
    #(#\x1e50 #\x1e52)
    #(#\x1e51 #\x1e53)
    #(#\x1e64)
    #(#\x1e65)
    #(#\x1e66)
    #(#\x1e67)
    #(#\x1e78)
    #(#\x1e79)
    #(#\x1e7a)
    #(#\x1e7b)
    #(#\x1e9b)
    #(#\x1edc #\x1eda #\x1ee0 #\x1ede #\x1ee2)
    #(#\x1edd #\x1edb #\x1ee1 #\x1edf #\x1ee3)
    #(#\x1eea #\x1ee8 #\x1eee #\x1eec #\x1ef0)
    #(#\x1eeb #\x1ee9 #\x1eef #\x1eed #\x1ef1)
    #(#\x1ee)
    #(#\x1ec)
    #(#\x1ed)
    #(#\x1e0)
    #(#\x1e1)
    #(#\x1e1c)
    #(#\x1e1d)
    #(#\x230)
    #(#\x231)
    #(#\x1ef)
    #(#\x1fba #\x386 #\x1fb9 #\x1fb8 #\x1f08 #\x1f09 #\x1fbc)
    #(#\x1fc8 #\x388 #\x1f18 #\x1f19)
    #(#\x1fca #\x389 #\x1f28 #\x1f29 #\x1fcc)
    #(#\x1fda #\x38a #\x1fd9 #\x1fd8 #\x3aa #\x1f38 #\x1f39)
    #(#\x1ff8 #\x38c #\x1f48 #\x1f49)
    #(#\x1fec)
    #(#\x1fea #\x38e #\x1fe9 #\x1fe8 #\x3ab #\x1f59)
    #(#\x1ffa #\x38f #\x1f68 #\x1f69 #\x1ffc)
    #(#\x1fb4)
    #(#\x1fc4)
    #(#\x1f70 #\x3ac #\x1fb1 #\x1fb0 #\x1f00 #\x1f01 #\x1fb6 #\x1fb3)
    #(#\x1f72 #\x3ad #\x1f10 #\x1f11)
    #(#\x1f74 #\x3ae #\x1f20 #\x1f21 #\x1fc6 #\x1fc3)
    #(#\x1f76 #\x3af #\x1fd1 #\x1fd0 #\x3ca #\x1f30 #\x1f31 #\x1fd6)
    #(#\x1f78 #\x3cc #\x1f40 #\x1f41)
    #(#\x1fe4 #\x1fe5)
    #(#\x1f7a #\x3cd #\x1fe1 #\x1fe0 #\x3cb #\x1f50 #\x1f51 #\x1fe6)
    #(#\x1f7c #\x3ce #\x1f60 #\x1f61 #\x1ff6 #\x1ff3)
    #(#\x1fd2 #\x390 #\x1fd7)
    #(#\x1fe2 #\x3b0 #\x1fe7)
    #(#\x1ff4)
    #(#\x3d3 #\x3d4)
    #(#\x407)
    #(#\x4d0 #\x4d2)
    #(#\x403)
    #(#\x400 #\x4d6 #\x401)
    #(#\x4c1 #\x4dc)
    #(#\x4de)
    #(#\x40d #\x4e2 #\x419 #\x4e4)
    #(#\x40c)
    #(#\x4e6)
    #(#\x4ee #\x40e #\x4f0 #\x4f2)
    #(#\x4f4)
    #(#\x4f8)
    #(#\x4ec)
    #(#\x4d1 #\x4d3)
    #(#\x453)
    #(#\x450 #\x4d7 #\x451)
    #(#\x4c2 #\x4dd)
    #(#\x4df)
    #(#\x45d #\x4e3 #\x439 #\x4e5)
    #(#\x45c)
    #(#\x4e7)
    #(#\x4ef #\x45e #\x4f1 #\x4f3)
    #(#\x4f5)
    #(#\x4f9)
    #(#\x4ed)
    #(#\x457)
    #(#\x476)
    #(#\x477)
    #(#\x4da)
    #(#\x4db)
    #(#\x4ea)
    #(#\x4eb)
    #(#\x622 #\x623 #\x625)
    #(#\x624)
    #(#\x626)
    #(#\x6c2)
    #(#\x6d3)
    #(#\x6c0)
    #(#\x929)
    #(#\x931)
    #(#\x934)
    #(#\x9cb #\x9cc)
    #(#\xb4b #\xb48 #\xb4c)
    #(#\xb94)
    #(#\xbca #\xbcc)
    #(#\xbcb)
    #(#\xc48)
    #(#\xcc0)
    #(#\xcca #\xcc7 #\xcc8)
    #(#\xccb)
    #(#\xd4a #\xd4c)
    #(#\xd4b)
    #(#\xdda #\xddc #\xdde)
    #(#\xddd)
    #(#\x1026)
    #(#\x1b06)
    #(#\x1b08)
    #(#\x1b0a)
    #(#\x1b0c)
    #(#\x1b0e)
    #(#\x1b12)
    #(#\x1b3b)
    #(#\x1b3d)
    #(#\x1b40)
    #(#\x1b41)
    #(#\x1b43)
    #(#\x1e38)
    #(#\x1e39)
    #(#\x1e5c)
    #(#\x1e5d)
    #(#\x1e68)
    #(#\x1e69)
    #(#\x1eac #\x1eb6)
    #(#\x1ead #\x1eb7)
    #(#\x1ec6)
    #(#\x1ec7)
    #(#\x1ed8)
    #(#\x1ed9)
    #(#\x1f02 #\x1f04 #\x1f06 #\x1f80)
    #(#\x1f03 #\x1f05 #\x1f07 #\x1f81)
    #(#\x1f82)
    #(#\x1f83)
    #(#\x1f84)
    #(#\x1f85)
    #(#\x1f86)
    #(#\x1f87)
    #(#\x1f0a #\x1f0c #\x1f0e #\x1f88)
    #(#\x1f0b #\x1f0d #\x1f0f #\x1f89)
    #(#\x1f8a)
    #(#\x1f8b)
    #(#\x1f8c)
    #(#\x1f8d)
    #(#\x1f8e)
    #(#\x1f8f)
    #(#\x1f12 #\x1f14)
    #(#\x1f13 #\x1f15)
    #(#\x1f1a #\x1f1c)
    #(#\x1f1b #\x1f1d)
    #(#\x1f22 #\x1f24 #\x1f26 #\x1f90)
    #(#\x1f23 #\x1f25 #\x1f27 #\x1f91)
    #(#\x1f92)
    #(#\x1f93)
    #(#\x1f94)
    #(#\x1f95)
    #(#\x1f96)
    #(#\x1f97)
    #(#\x1f2a #\x1f2c #\x1f2e #\x1f98)
    #(#\x1f2b #\x1f2d #\x1f2f #\x1f99)
    #(#\x1f9a)
    #(#\x1f9b)
    #(#\x1f9c)
    #(#\x1f9d)
    #(#\x1f9e)
    #(#\x1f9f)
    #(#\x1f32 #\x1f34 #\x1f36)
    #(#\x1f33 #\x1f35 #\x1f37)
    #(#\x1f3a #\x1f3c #\x1f3e)
    #(#\x1f3b #\x1f3d #\x1f3f)
    #(#\x1f42 #\x1f44)
    #(#\x1f43 #\x1f45)
    #(#\x1f4a #\x1f4c)
    #(#\x1f4b #\x1f4d)
    #(#\x1f52 #\x1f54 #\x1f56)
    #(#\x1f53 #\x1f55 #\x1f57)
    #(#\x1f5b #\x1f5d #\x1f5f)
    #(#\x1f62 #\x1f64 #\x1f66 #\x1fa0)
    #(#\x1f63 #\x1f65 #\x1f67 #\x1fa1)
    #(#\x1fa2)
    #(#\x1fa3)
    #(#\x1fa4)
    #(#\x1fa5)
    #(#\x1fa6)
    #(#\x1fa7)
    #(#\x1f6a #\x1f6c #\x1f6e #\x1fa8)
    #(#\x1f6b #\x1f6d #\x1f6f #\x1fa9)
    #(#\x1faa)
    #(#\x1fab)
    #(#\x1fac)
    #(#\x1fad)
    #(#\x1fae)
    #(#\x1faf)
    #(#\x1fb2)
    #(#\x1fc2)
    #(#\x1ff2)
    #(#\x1fb7)
    #(#\x1fcd #\x1fce #\x1fcf)
    #(#\x1fc7)
    #(#\x1ff7)
    #(#\x1fdd #\x1fde #\x1fdf)
    #(#\x219a)
    #(#\x219b)
    #(#\x21ae)
    #(#\x21cd)
    #(#\x21cf)
    #(#\x21ce)
    #(#\x2204)
    #(#\x2209)
    #(#\x220c)
    #(#\x2224)
    #(#\x2226)
    #(#\x2241)
    #(#\x2244)
    #(#\x2247)
    #(#\x2249)
    #(#\x226d)
    #(#\x2262)
    #(#\x2270)
    #(#\x2271)
    #(#\x2274)
    #(#\x2275)
    #(#\x2278)
    #(#\x2279)
    #(#\x2280)
    #(#\x2281)
    #(#\x22e0)
    #(#\x22e1)
    #(#\x2284)
    #(#\x2285)
    #(#\x2288)
    #(#\x2289)
    #(#\x22e2)
    #(#\x22e3)
    #(#\x22ac)
    #(#\x22ad)
    #(#\x22ae)
    #(#\x22af)
    #(#\x22ea)
    #(#\x22eb)
    #(#\x22ec)
    #(#\x22ed)
    #(#\x3094)
    #(#\x304c)
    #(#\x304e)
    #(#\x3050)
    #(#\x3052)
    #(#\x3054)
    #(#\x3056)
    #(#\x3058)
    #(#\x305a)
    #(#\x305c)
    #(#\x305e)
    #(#\x3060)
    #(#\x3062)
    #(#\x3065)
    #(#\x3067)
    #(#\x3069)
    #(#\x3070 #\x3071)
    #(#\x3073 #\x3074)
    #(#\x3076 #\x3077)
    #(#\x3079 #\x307a)
    #(#\x307c #\x307d)
    #(#\x309e)
    #(#\x30f4)
    #(#\x30ac)
    #(#\x30ae)
    #(#\x30b0)
    #(#\x30b2)
    #(#\x30b4)
    #(#\x30b6)
    #(#\x30b8)
    #(#\x30ba)
    #(#\x30bc)
    #(#\x30be)
    #(#\x30c0)
    #(#\x30c2)
    #(#\x30c5)
    #(#\x30c7)
    #(#\x30c9)
    #(#\x30d0 #\x30d1)
    #(#\x30d3 #\x30d4)
    #(#\x30d6 #\x30d7)
    #(#\x30d9 #\x30da)
    #(#\x30dc #\x30dd)
    #(#\x30f7)
    #(#\x30f8)
    #(#\x30f9)
    #(#\x30fa)
    #(#\x30fe)
    #(#\x1109a)
    #(#\x1109c)
    #(#\x110ab)
    #(#\x1112e)
    #(#\x1112f)
    #(#\x1134b #\x1134c)
    #(#\x114bc #\x114bb #\x114be)
    #(#\x115ba)
    #(#\x115bb)
    #(#\x11938)))
