/*
 * Copyright 2007-2009 Sun Microsystems, Inc.  All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *   - Neither the name of Sun Microsystems nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package VASSAL.tools.nio.file.zipfs;

public class ZipHeaderConstants {

  public static final int LOCAL_FILE_HDR_SIG = 0x04034b50;
  public static final int CENTRAL_FILE_HDR_SIG = 0x02014b50;
  public static final int END_CENTRAL_HDR_SIG = 0x06054b50;

  // All offsets are in bytes

  // Local File Header offsets
  public static final long COMP_METHOD_OFF = 8;
  public static final long COMP_SIZE_OFF = 18;
  public static final long UCOMP_SIZE_OFF = COMP_SIZE_OFF + 4;
  public static final long FILE_NAME_LEN_OFF = UCOMP_SIZE_OFF + 4;
  public static final long EXTRA_FLD_LEN_OFF = FILE_NAME_LEN_OFF + 2;
  public static final long FILE_NAME_OFF =  EXTRA_FLD_LEN_OFF + 2;

  // File Header of Central Directory Structure
  public static final long C_VER_MADE_BY  = 4;
  public static final long C_COMP_METHOD_OFF = COMP_METHOD_OFF + 2;
  public static final long C_LAST_MOD_TIME_OFF = C_COMP_METHOD_OFF + 2;
  public static final long C_LAST_MOD_DATE_OFF = C_LAST_MOD_TIME_OFF + 2;
  public static final long C_CRC_OFF =  C_LAST_MOD_DATE_OFF + 2;
  public static final long C_COMP_SIZE_OFF =  C_CRC_OFF + 4;
  public static final long C_UCOMP_SIZE_OFF = C_COMP_SIZE_OFF + 4;
  public static final long C_FILE_NAME_LEN_OFF = C_UCOMP_SIZE_OFF + 4;
  public static final long C_EXTRA_FLD_LEN_OFF = C_FILE_NAME_LEN_OFF + 2;
  public static final long C_COMMENT_LEN_OFF = C_EXTRA_FLD_LEN_OFF + 2;
  public static final long C_DISK_NO_OFF = C_COMMENT_LEN_OFF + 2;
  public static final long C_INT_ATTR_OFF = C_DISK_NO_OFF + 2;
  public static final long C_EXT_ATTR_OFF = C_INT_ATTR_OFF + 2;
  public static final long C_REL_OFF_LOCAL_HDR_OFF = C_EXT_ATTR_OFF + 4;
  public static final long C_FILE_NAME_OFF =  C_REL_OFF_LOCAL_HDR_OFF + 4;

  // End of Central directory Record
  public static final int C_COMMENT_LEN_BLEN = 2;
  public static final int C_EXT_ATTR_BLEN = 4;

  public static final long C_TOTAL_ENTRIES_OFF =10;
  public static final int C_DIR_START_OFF = 16;

  public static final int C_END_RECORD_MIN_OFF = 19;
}
