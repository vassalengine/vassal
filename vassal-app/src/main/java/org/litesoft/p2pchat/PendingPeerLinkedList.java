package org.litesoft.p2pchat;

import java.net.Socket;


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
 * @version 0.4 04/06/13 Return if interrupted on wait() to allow PPM to clean up
 * @version 0.3 02/02/02 Added IllegalArgument.ifNull for all public params that may not be null
 * @version 0.2 01/28/02 Refactored and Added Licence
 * @version 0.1 12/27/01 Initial Version
 */
public class PendingPeerLinkedList {
  private PendingPeerNode zHead = null;
  private PendingPeerNode zTail = null;

  public PendingPeerLinkedList() {
  }

  public synchronized PendingPeerNode next() {
    while (zHead == null) {
      try {
        wait();
      }
      catch (InterruptedException e) {
        // PPM has asked us to stop. It will handle the null return value
        return null;
      }
    }
    PendingPeerNode retval = zHead;
    zHead = zHead.advanceToNext();
    if (zHead == null)
      zTail = null;
    return retval;
  }

  public synchronized void add(Socket pSocket, PeerInfo pInfo) {    
    IllegalArgument.ifNull("Info", pInfo);
    add(new PendingPeerNode(pSocket, pInfo));
  }

  public synchronized void add(PendingPeerNode pPendingPeerNode) {
    if (zHead == null)
      zHead = zTail = pPendingPeerNode;
    else
      zTail = zTail.appendNode(pPendingPeerNode);
    notifyAll();
  }
  

}