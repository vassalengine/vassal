package org.litesoft.p2pchat;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.Vector;

// Copyright Status:
//
// All Software available from LiteSoft.org (including this file) is
// hereby released into the public domain.
//
// It is free!  As in, you may use it freely in both commercial and
// non-commercial applications, bundle it with your software
// distribution, include it on a CD-ROM, list the source code in a book,
// mirror the documentation at your own web site, or use it in any other
// way you see fit.
//
// NO Warranty!
//
// All software is provided "as is".
//
// There is ABSOLUTELY NO WARRANTY OF ANY KIND: not for the design, fitness
// (for a particular purpose), level of errors (or lack thereof), or
// applicability of this software.  The entire risk as to the quality
// and performance of this software is with you.  Should this software
// prove defective, you assume the cost of all necessary servicing, repair
// or correction.
//
// In no event unless required by applicable law or agreed to in writing
// will any party who created or may modify and/or redistribute this
// software, be liable to you for damages, including any general,
// special, incidental or consequential damages arising out of the use or
// inability to use this software (including but not limited to loss of
// data or data being rendered inaccurate or losses sustained by you or
// third parties or a failure of this software to operate with any
// other programs), even if such holder or other party has been advised
// of the possibility of such damages.
//
// NOTE: Should you discover a bug, have a recogmendation for a change, wish
// to submit modifications, or wish to add new classes/functionality,
// please email them to:
//
//        changes@litesoft.org
//

/**
 * @author  Devin Smith and George Smith
 * @version 0.3 05/17/13 Specify UTF-8 charset
 * @version 0.2 04/20/13 Add thread name, change history.
 * @version 0.1 12/27/01 Initial Version
 */

public class PeerReader extends Thread {
  private Vector<String> zLines = new Vector<String>();
  private BufferedReader zReader;
  private boolean isOpen = true;

  public PeerReader(InputStream pIs) {
    IllegalArgument.ifNull("Is", pIs);
    zReader = new BufferedReader(new InputStreamReader(pIs,Charset.forName("UTF-8")));
    setName("Peer Reader Thread");
    start();
  }

  public synchronized void close() {
    isOpen = false;
    notifyAll();
  }

  public synchronized boolean isOpen() {
    return isOpen;
  }

  private synchronized void add(String pMessage) {
    if (isOpen) {
      zLines.addElement(pMessage);
    }
    notifyAll();
  }

  /* Blocks until a line is available to read */
  public synchronized String readLine() {
    while (isOpen && zLines.isEmpty()) {
      try {
        wait();
      }
      catch (InterruptedException e) {
      }
    }
    String message = null;
    if (isOpen && zLines.size() > 0) { // size can be zero if we called close()
      message = (String) zLines.elementAt(0);
      zLines.removeElementAt(0);
    }
    return message;
  }

  public void run() {
    String line;
    try {
      while (null != (line = zReader.readLine())) {
        add(line);
        if (line.equals("BYE")) {
          break;
        }
      }
    }
    catch (Exception ignoreBecauseWeAssumeSocketClosed) {
    }
    isOpen = false;
    try {
      zReader.close();
    }
    catch (IOException ignore) {
    }
  }
}
