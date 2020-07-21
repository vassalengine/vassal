/*
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.launch;

import VASSAL.tools.concurrent.listener.EventListener;
import VASSAL.tools.image.tilecache.ZipFileImageTiler;

/**
 * A state machine for parsing the output of {@link ZipFileImageTiler}.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
class TileProgressPumpStateMachine {

  protected final EventListener<String> nameListener;
  protected final EventListener<Integer> progListener;

  /**
   * Creates a <code>TileProgressPumpStateMachine</code>.
   *
   * @param nameListener the listener for new filename events
   * @param progListener the listener for progress events
   */
  public TileProgressPumpStateMachine(EventListener<String> nameListener,
                                      EventListener<Integer> progListener) {
    this.nameListener = nameListener;
    this.progListener = progListener;
  }

  public static final int INIT    = 0;
  public static final int NAME    = 1;
  public static final int NAME_LF = 2;
  public static final int DOTS    = 3;
  public static final int DOTS_LF = 4;
  public static final int DONE    = 5;

  protected void appendName(StringBuilder sb, byte[] buf, int beg, int end) {
    sb.append(new String(buf, beg, end-beg));
  }

  protected boolean hasName(StringBuilder sb) {
    return sb.length() > 0;
  }

  protected void sendName(StringBuilder sb) {
    nameListener.receive(this, sb.toString());
    sb.setLength(0);
  }

  protected void sendProgress(int prog) {
    progListener.receive(this, prog);
  }

  protected int[] runName(byte[] buf, int beg, int end, StringBuilder sb) {
    // look for end of line
    for (int pos = beg; pos < end; ++pos) {
      if (buf[pos] == '\r' || buf[pos] == '\n') {
        // found the end of line

        // terminate if the buffer is empty
        if (pos == beg && !hasName(sb)) {
          return new int[] { DONE, end };
        }

        // otherwise, send the buffer up to this position as the filename
        appendName(sb, buf, beg, pos);
        sendName(sb);

        // found a carriage return
        if (buf[pos] == '\r') {
          // now look for a linefeed
          return new int[] { NAME_LF, pos+1 };
        }
        else {
          // now look for dots
          return new int[] { DOTS, pos+1 };
        }

        // found a regular character, keep looking
      }
    }

    // exhausted the buffer without finding end of line

    // store the pratial filename we've read
    appendName(sb, buf, beg, end);

    // continue looking for end of line
    return new int[] { NAME, end };
  }

  protected int[] runNameLF(byte[] buf, int beg, int end, StringBuilder sb) {
    // look for a linefeed
    switch (buf[beg]) {
    case '\n':
      // found a linefeed
      // now look for dots
      return new int[] { DOTS, beg+1 };

    default:
      // found something else, protocol violation
      throw new IllegalStateException(
        "found '" + buf[beg] + "', expecting '\\n'");
    }
  }

  protected int[] runDots(byte[] buf, int beg, int end, StringBuilder sb) {
    // look for end of line
    for (int pos = beg; pos < end; ++pos) {
      switch (buf[pos]) {
      case '\r':
      case '\n':
        // found the end of line

        // send the buffer up to this position as the progress
        sendProgress(pos-beg);

        // found a carriage return
        if (buf[pos] == '\r') {
          // now look for a linefeed
          return new int[] { DOTS_LF, pos+1 };
        }
        else {
          // now look for filename
          return new int[] { NAME, pos+1 };
        }

      case '.':
        // found a progress dot, keep looking
        break;

      default:
        // found somethine else, protocol violation
        throw new IllegalStateException(
          "found '" + buf[pos] + "', expecting '.'");
      }
    }

    // exhausted the buffer without finding end of line

    // send the progress to this point
    sendProgress(end-beg);

    // continue looking for end of line
    return new int[] { DOTS, end };
  }

  protected int[] runDotsLF(byte[] buf, int beg, int end, StringBuilder sb) {
    // look for a linefeed
    switch (buf[beg]) {
    case '\n':
      // found a linefeed
      // now look for filename
      return new int[] { NAME, beg+1 };

    default:
      // found something else, protocol violation
      throw new IllegalStateException(
        "found '" + buf[beg] + "', expecting '\\n'");
    }
  }

  /**
   * Run the state machine to the end of the buffer.
   *
   * @param state the current state
   * @param buf the byte buffer to read
   * @param beg the beginning position in the buffer (inclusive)
   * @param end the the ending position in the buffer (exclusive)
   * @param sb the string builder for holding name
   */
  public int run(int state, byte[] buf, int beg, int end, StringBuilder sb) {
    if (state == DONE) {
      throw new IllegalArgumentException("DONE is terminal");
    }

    if (state == INIT) {
      state = NAME;
    }

    while (beg < end) {
      int[] result;

      switch (state) {
      case NAME:    result = runName(  buf, beg, end, sb); break;
      case NAME_LF: result = runNameLF(buf, beg, end, sb); break;
      case DOTS:    result = runDots(  buf, beg, end, sb); break;
      case DOTS_LF: result = runDotsLF(buf, beg, end, sb); break;

      default:
        // should never happen
        throw new IllegalStateException("state == " + state);
      }

      state = result[0];
      beg = result[1];
    }

    return state;
  }
}
