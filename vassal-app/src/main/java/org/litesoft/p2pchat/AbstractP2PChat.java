package org.litesoft.p2pchat;

import java.io.*;
import java.net.*;


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
 * @version 0.3 02/02/02 Added IllegalArgument.ifNull for all public params that may not be null
 * @version 0.2 01/28/02 Refactored and Added Licence
 * @version 0.1 12/27/01 Initial Version
 */
public abstract class AbstractP2PChat {
  private static final String VERSION = "0.2";

  private static final int DEFAULTPORT = 11581;

  protected static String getTitle() {
    return "P2PChat ver " + VERSION;
  }

  protected static void dumpHelp(String problem) {
    System.out.println(getTitle());
    System.out.println();
    System.out.println(problem);
    System.out.println();
    System.out.println("This program takes 1-n arguments:");
    System.out.println("   1st - Chatname");
    System.out.println("   nth - our Address and/or port");
    System.out.println("         peer(s) Address and optional port");
    System.out.println();
    System.out.println("   An Address is a standard dotted address (eg 192.168.1.5)");
    System.out.println("   A Port is a simple integer (default: " + DEFAULTPORT + ")");
    System.out.println("   A colon (':') seperates the address and port (eg 192.168.1.5:5432)");
    System.out.println("   Our Address and/or port is indictaed by being surrounded by square");
    System.out.println("      brackets (eg [192.168.1.5:5432])");
    System.exit(0);
  }

  abstract protected UserDialog getUserDialog(MyInfo pMyInfo);

  protected void init(String[] args) {
    Pars parser = new Pars();

    String error = parser.parse(args);

    if (error != null)
      dumpHelp(error);

    try {
      UserDialog userDialog = getUserDialog(parser.zMyInfo);

      PendingPeerManager ppm = new PendingPeerManager(userDialog);
      PeerInfo[] initialPeers = parser.zInitialPeers;
      if (initialPeers != null)
        for (int i = 0; i < initialPeers.length; i++)
          ppm.addNewPeer(initialPeers[i]);

      ServerSocket serverSocket = getServerSocket(parser.zMyInfo.getPort());

      new ActivePeerManager(parser.zMyInfo, userDialog, ppm);

      while (true) {
        ppm.addNewPeer(serverSocket.accept());
      }
    }
    catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
  }

  private ServerSocket getServerSocket(int pPort) {
    try {
      return new ServerSocket(pPort);
    }
    catch (IOException e) {
      dumpHelp("Unable to Open Socket Listener on Port number(" + pPort + ").  Error: " + e.getMessage());
    }
    return null;// Not Reachable
  }

  // An Address is a standard dotted address (eg 192.168.1.5)
  // A Port is a simple integer
  // A colon (':') seperates the address and port (eg 192.168.1.5:5432)
  // Our's is indictaed by surrounding square brackets (eg [192.168.1.5:5432])
  private static class Pars {
    public MyInfo zMyInfo = null;
    public PeerInfo[] zInitialPeers = null;

    public String parse(String[] args) {
      if ((args == null) || (args.length == 0)) {
        return "Not enough parameters.";
      }

      String chatName = args[0].trim();
      if (chatName.length() == 0) {
        return "No chatname specified.";
      }

      for (int i = 1; i < args.length; i++) {
        String err , s = args[i];
        if (s.startsWith("[") && s.endsWith("]"))
          err = parseMyInfo(chatName, s.substring(1, s.length() - 1));
        else
          err = parsePeerInfo(s);
        if (err != null)
          return err;
      }
      if (zMyInfo == null)
        zMyInfo = new MyInfo(chatName, (String) null, DEFAULTPORT);
      return null;
    }

    private String parseMyInfo(String pChatName, String pParm) {
      if (zMyInfo != null)
        return "My Info (" + zMyInfo + ") already given.";

      Port ap = new Port();

      String err = ap.parse(pParm);

      if (err != null)
        return err;

      zMyInfo = new MyInfo(pChatName, ap.zAddress, ap.zPort);

      return null; // No Error
    }

    private String parsePeerInfo(String pParm) {
      Port ap = new Port();

      String err = ap.parse(pParm);

      if (err != null)
        return err;

      if (ap.zAddress == null)
        return "Peer Address not found in: " + pParm;

      if (zInitialPeers == null)
        zInitialPeers = new PeerInfo[1];
      else {
        PeerInfo[] temp = new PeerInfo[zInitialPeers.length + 1];
        System.arraycopy(zInitialPeers, 0, temp, 0, zInitialPeers.length);
        zInitialPeers = temp; // From   , At , To   , At , For
      }
      zInitialPeers[zInitialPeers.length - 1] = new PeerInfo(null, ap.zAddress, ap.zPort);

      return null;
    }

    // An Address is a standard dotted address (eg 192.168.1.5)
    // A Port is a simple integer
    // A colon (':') seperates the address and port (eg 192.168.1.5:5432)
    // Our's is indictaed by surrounding square brackets (eg [192.168.1.5:5432])
    private static class Port {
      public String zAddress = null;
      public int zPort = DEFAULTPORT;

      public String parse(String pParm) {
        IllegalArgument.ifNull("Parm", pParm);

        String err;
        int colonAt = (pParm = pParm.trim()).indexOf(':');

        if (colonAt != -1) {
          if (null != (err = parseAddress(pParm.substring(0, colonAt))))
            return "Invalid Address in (" + pParm + "): " + err;
          if (null != (err = parsePort(pParm.substring(colonAt + 1))))
            return "Invalid Port in (" + pParm + "): " + err;
          return null;
        }
        if (isAllDigits(pParm)) {
          if (null != (err = parsePort(pParm)))
            return "Port (" + pParm + ") Invalid: " + err;
        }
        else {
          if (null != (err = parseAddress(pParm)))
            return "Address (" + pParm + ") Invalid: " + err;
        }
        return null;
      }

      private String parsePort(String pParm) {
        try {
          zPort = Integer.parseInt(pParm.trim());
        }
        catch (NumberFormatException e) {
          return "Not a number.";
        }
        if (zPort < 1024 || zPort >= (2 << 16))
          return "Not within range. Must be a 16 bit value greater then 1024";
        return null;
      }

      private String parseAddress(String pParm) {
        if ((pParm = pParm.trim()).length() == 0) {
          zAddress = MyInfo.getIPs();
          return null;
        }
        for (int commaAt; -1 != (commaAt = pParm.indexOf(',')); pParm = pParm.substring(commaAt + 1).trim()) {
          String err = parseAnAddress(pParm.substring(0, commaAt));
          if (err != null)
            return err;
        }
        return parseAnAddress(pParm);
      }

      private String parseAnAddress(String pParm) {
        int dotAt = pParm.lastIndexOf('.');

        if (dotAt == -1)
          return "No dot ('.') in: " + pParm;

        String lastField = pParm.substring(dotAt + 1);

        if (isAllDigits(lastField))
          return parseDirectAddress(pParm);

        if (lastField.length() == 0)
          return "No TLD in: " + pParm;

        if (-1 != pParm.indexOf(' '))
          return "Illegal Domain Reference in: " + pParm;

        String[] directAddresses = convertAddressDomainToDirects(pParm);

        if (directAddresses == null)
          return "Unresolvable Domain Reference: " + pParm;

        for (int i = 0; i < directAddresses.length; i++)
          addAddress(directAddresses[i]);

        return null;
      }

      private void addAddress(String pAddress) {
        if (zAddress == null)
          zAddress = pAddress;
        else
          zAddress += "," + pAddress;
      }

      private boolean isAllDigits(String pParm) {
        if (pParm.length() == 0)
          return false;
        for (int i = pParm.length(); i-- > 0;)
          if (!Character.isDigit(pParm.charAt(i)))
            return false;
        return true;
      }

      private String[] convertAddressDomainToDirects(String pParm) {
        InetAddress[] addresses;
        try {
          addresses = InetAddress.getAllByName(pParm);
        }
        catch (UnknownHostException e) {
          return null;
        }
        if ((addresses == null) || (addresses.length == 0))
          return null;

        String[] retval = new String[addresses.length];
        for (int i = 0; i < addresses.length; i++)
          retval[i] = addresses[i].toString();

        return retval;
      }

      private String parseDirectAddress(String pParm) {
        Dots da = new Dots(pParm);
        for (int i = 0; i < da.zParts.length; i++)
          if (!isAllDigits(da.zParts[i]))
            return "Non-Numeric (" + da.zParts[i] + ") Dotted Address in: " + pParm;

        if (da.zParts.length > 4)
          return "Too Many Parts in Address: " + pParm;

        if (da.zParts.length == 4) {
          addAddress(da.toString());
          return null;
        }

        String s;
        for (int i = 0; null != (s = ThisMachine.getIPAddress(i)); i++) {
          Dots us = new Dots(s);
          us.overlayFrom(da);
          addAddress(us.toString());
        }
        return null;
      }
    }

    private static class Dots {
      public String[] zParts = null; // Back to Front

      public Dots(String pParm) {
        if (pParm == null)
          return;

        zParts = new String[0];

        while (pParm.startsWith("."))
          pParm = pParm.substring(1);

        if (pParm.length() == 0)
          return;

        for (int dotAt; -1 != (dotAt = pParm.indexOf('.')); pParm = pParm.substring(dotAt + 1))
          addPart(pParm.substring(0, dotAt));
        addPart(pParm);
      }

      private void addPart(String pPart) {
        if (zParts.length == 0)
          zParts = new String[1];
        else {
          String[] temp = new String[zParts.length + 1];
          System.arraycopy(zParts, 0, temp, 1, zParts.length);
          zParts = temp; // From   , At , To   , At , For
        }
        zParts[0] = pPart;
      }

      public void overlayFrom(Dots pParm) {
        if (pParm != null)
          for (int i = 0; i < pParm.zParts.length; i++)
            this.zParts[i] = pParm.zParts[i];
      }

      public String toString() {
        if (zParts.length == 0)
          return "No Address";
        StringBuffer sb = new StringBuffer(16);
        for (int i = zParts.length; i-- > 0;) {
          sb.append('.');
          sb.append(zParts[i]);
        }
        return sb.toString().substring(1);
      }
    }
  }
}
