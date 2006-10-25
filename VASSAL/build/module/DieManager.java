/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Brent Easton
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
package VASSAL.build.module;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.URL;
import java.net.URLConnection;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Vector;
import javax.swing.DefaultListModel;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.preferences.Prefs;
import VASSAL.tools.BackgroundTask;
import VASSAL.tools.FormattedString;

/**
 * @author Brent Easton
 *
 * Internet Die Roller Manager. Includes all the smarts to interface to web-based
 * Die Servers
 */

public class DieManager extends AbstractConfigurable {

  private Hashtable servers;
  private Vector dieButtons = new Vector();
  private String desc = "Die Manager";
  private boolean useMultiRoll;
  private int defaultNDice = 2;
  private int defaultNSides = 6;

  private DieServer server;
  private String lastServerName = "";
  private MultiRoll myMultiRoll;
  final StringEnumConfigurer semail;

  public static final String USE_INTERNET_DICE = "useinternetdice";
  public static final String DICE_SERVER = "diceserver";
  public static final String SERVER_PW = "serverpw";
  public static final String USE_EMAIL = "useemail";
  public static final String PRIMARY_EMAIL = "primaryemail";
  public static final String SECONDARY_EMAIL = "secondaryemail";
  public static final String ADDRESS_BOOK = "addressbook";
  public static final String MULTI_ROLL = "multiroll";
  public static final String DIE_MANAGER = "Internet Die Roller";

  public static final String ROLL_MARKER = "VASSAL auto-generated dice roll";

  public static final String DESC = "description";
  public static final String DFLT_NSIDES = "dfltnsides";
  public static final String DFLT_NDICE = "dfltndice";

  public DieManager() {

    DieServer d;
    servers = new Hashtable();

    /*
     * Create the Internet Dice Servers we know about
     */
//    d = new InbuiltDieServer();
//    servers.put(d.getName(), d);
//    server = d; // Set the default Internet Server

//        d = new IronyDieServer();
//        servers.put(d.getName(), d);
//
//        d = new InternetGamesDieServer();
//        servers.put(d.getName(), d);

    d = new ShadowDiceDieServer();
    servers.put(d.getName(), d);
    server = d;

    /*
     * The Dice Manager needs some preferences
     */

    final StringEnumConfigurer dieserver = new StringEnumConfigurer(DICE_SERVER, "Internet Dice Server", getDescriptions());
    dieserver.setValue(server.getDescription());
    final StringConfigurer serverpw = new StringConfigurer(SERVER_PW, "Dice Server Password");
    final BooleanConfigurer useemail = new BooleanConfigurer(USE_EMAIL, "Email results?");
    final StringConfigurer pemail = new StringConfigurer(PRIMARY_EMAIL, "Primary Email");
    final StringArrayConfigurer abook = new StringArrayConfigurer(ADDRESS_BOOK, "Address Book");
    final BooleanConfigurer multiroll = new BooleanConfigurer(MULTI_ROLL, "Put multiple rolls into single email");

    GameModule.getGameModule().getPrefs().addOption(null, dieserver);
    GameModule.getGameModule().getPrefs().addOption(null, serverpw);
    GameModule.getGameModule().getPrefs().addOption(DIE_MANAGER, useemail);

    GameModule.getGameModule().getPrefs().addOption(DIE_MANAGER, abook);
    String[] addressList = (String[]) GameModule.getGameModule().getPrefs().getValue(ADDRESS_BOOK);
    semail = new StringEnumConfigurer(SECONDARY_EMAIL, "Secondary Email", addressList);

    GameModule.getGameModule().getPrefs().addOption(DIE_MANAGER, pemail);
    GameModule.getGameModule().getPrefs().addOption(DIE_MANAGER, semail);
    GameModule.getGameModule().getPrefs().addOption(DIE_MANAGER, multiroll);

    setSemailValues();

    DefaultListModel m = abook.getModel();
    ListDataListener ldl = new ListDataListener() {

      public void contentsChanged(ListDataEvent arg0) {
        setSemailValues();
      }

      public void intervalAdded(ListDataEvent arg0) {
        setSemailValues();
      }

      public void intervalRemoved(ListDataEvent arg0) {
        setSemailValues();
      }
    };
    m.addListDataListener(ldl);
  }

  public void setSemailValues() {
    String currentSemail = (String) GameModule.getGameModule().getPrefs().getValue(SECONDARY_EMAIL);
    String[] addressBook = (String[]) GameModule.getGameModule().getPrefs().getValue(ADDRESS_BOOK);
    semail.setValidValues(addressBook);
    semail.setValue(currentSemail);
  }

  // Return names of all known Dice Servers
  public String[] getNames() {
    if (servers == null) {
      return null;
    }
    else {
      String s[] = new String[servers.size()];
      Enumeration e = servers.keys();

      for (int i = 0; e.hasMoreElements(); i++) {
        s[i] = (String) e.nextElement();
      }
      return s;
    }
  }

  // Return descriptions of all known dice servers
  public String[] getDescriptions() {
    if (servers == null) {
      return null;
    }
    else {
      String s[] = new String[servers.size()];
      Enumeration e = servers.elements();

      for (int i = 0; e.hasMoreElements(); i++) {
        s[i] = ((DieServer) e.nextElement()).getDescription();
      }
      return s;
    }
  }

  // Return server matching Name
  public DieServer getServerForName(String name) {
    return (DieServer) servers.get(name);
  }

  // Return server matching Description
  public DieServer getServerFromDescription(String de) {
    DieServer d = null;

    Enumeration e = servers.elements();
    while (e.hasMoreElements()) {
      d = (DieServer) e.nextElement();
      if (de.equals(d.getDescription())) {
        return d;
      }
    }
    return null;
  }

  public DieServer getServer() {
    getPrefs();
    return server;
  }

  public String getServerDescription() {
    return getServer().getDescription();
  }

  public String getServerName() {
    return getServer().getName();
  }

  public int getDfltNDice() {
    return defaultNDice;
  }

  public int getDfltNSides() {
    return defaultNSides;
  }

  public MultiRoll getMultiRoll(int nDice, int nSides) {
    String serverName = getServer().getName();
    if (myMultiRoll == null || !serverName.equals(lastServerName)) {
      myMultiRoll = new MultiRoll(this, nDice, nSides);
    }
    lastServerName = serverName;

    return myMultiRoll;
  }

  public void roll(int nDice, int nSides, int plus, boolean reportTotal, String description, FormattedString format) {
    MultiRoll mroll = getMultiRoll(nDice, nSides);
    getPrefs();

    RollSet rollSet;

    String desc = GameModule.getGameModule().getChatter().getInputField().getText();
    if (desc != null && desc.length() > 0) {
      mroll.setDescription(desc);
    }

    // Do we want full multi-roll capabilities? If required, pop-up the multi-roll
    // cofigurer to get the details
    if (useMultiRoll) {
      mroll.setVisible(true);

      if (mroll.wasCancelled()) {
        return;
      }
      rollSet = mroll.getRollSet();
      desc = rollSet.getDescription();
    }

    // Multi Roll preference not selected, so build a dummy MultiRoll object
    else {
      DieRoll[] rolls = new DieRoll[]{new DieRoll(description, nDice, nSides, plus, reportTotal)};
      rollSet = new RollSet(description, rolls);
      desc = "";
    }

    Command chatCommand = new Chatter.DisplayText(GameModule.getGameModule().getChatter(),
                                                  " - Roll sent to " + server.getDescription());

    if (desc == null || desc.length() == 0) {
      desc = GameModule.getGameModule().getChatter().getInputField().getText();
    }
    if (server.getUseEmail()) {
      if (desc == null || desc.length() == 0) {
        chatCommand.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(),
                                                   " - Emailing " + server.getSecondaryEmail() + " (no subject line)"));
        chatCommand.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(),
                                                   " - Leave text in the chat input area to provide a subject line"));
      }
      else {
        chatCommand.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(),
                                                   " - Emailing " + server.getSecondaryEmail() + " (Subject:  " + desc + ")"));
      }
    }
    chatCommand.execute();
    GameModule.getGameModule().sendAndLog(chatCommand);

    GameModule.getGameModule().getChatter().getInputField().setText("");
    rollSet.setDescription(desc);

    server.roll(rollSet, format);
  }

  /*
   * Retrieve the Dice Manager preferences and update the current Server
   * Preferences may change at ANY time!
   */
  private void getPrefs() {

    Prefs prefs = GameModule.getGameModule().getPrefs();

    // Get the correct server
    String serverName = ((String) prefs.getValue(DICE_SERVER));
    server = getServerFromDescription(serverName);

    // And tell it the prefs it will need
    server.setPasswd((String) prefs.getValue(SERVER_PW));
    server.setUseEmail(((Boolean) prefs.getValue(USE_EMAIL)).booleanValue());
    server.setPrimaryEmail((String) prefs.getValue(PRIMARY_EMAIL));
    server.setSecondaryEmail((String) prefs.getValue(SECONDARY_EMAIL));

    useMultiRoll = ((Boolean) prefs.getValue(MULTI_ROLL)).booleanValue();

  }

  /*
      public void addDie(SpecialDie d) {
          specialDice.add(d);
      }

      public void removeDie(SpecialDie d) {
          specialDice.remove(d);
      }
  */

  public void addDieButton(InternetDiceButton d) {
    dieButtons.add(d);
  }

  public void removeDieButton(InternetDiceButton d) {
    dieButtons.remove(d);
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Description", "Multi-roll Default Ndice", "Multi-roll Default Nsides"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, Integer.class, Integer.class};
  }

  public String[] getAttributeNames() {
    return new String[]{DESC, DFLT_NDICE, DFLT_NSIDES};
  }

  public void setAttribute(String key, Object value) {
    if (DESC.equals(key)) {
      desc = (String) value;
    }
    else if (DFLT_NDICE.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      defaultNDice = ((Integer) value).intValue();
    }
    else if (DFLT_NSIDES.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      defaultNSides = ((Integer) value).intValue();
    }
  }

  public String getAttributeValueString(String key) {
    if (DESC.equals(key)) {
      return desc;
    }
    else if (DFLT_NDICE.equals(key)) {
      return defaultNDice + "";
    }
    else if (DFLT_NSIDES.equals(key)) {
      return defaultNSides + "";
    }
    else
      return null;
  }

  public void removeFrom(Buildable parent) {
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{InternetDiceButton.class};
  }

  public void addTo(Buildable parent) {
  }

  public static String getConfigureTypeName() {
    return "Die Manager";
  }

  public void setSecondaryEmail(String email) {
    GameModule.getGameModule().getPrefs().setValue(SECONDARY_EMAIL, email);
    server.setSecondaryEmail(email);
  }

  /** Describes a set of {@link DieRoll}s */
  public static class RollSet {
    public String description;
    public DieRoll[] dieRolls;

    public RollSet(String description, DieRoll[] rolls) {
      this.description = description;
      this.dieRolls = rolls;
    }

    public String getDescription() {
      return description;
    }

    public void setDescription(String description) {
      this.description = description;
    }

    public DieRoll[] getDieRolls() {
      return dieRolls;
    }

    public int getMaxDescLength() {
      int len = 0;
      for (int i = 0; i < dieRolls.length; i++) {
        len = Math.max(len, dieRolls[i].getDescription().length());
      }
      return len;
    }
  }

  /**
   * Base DieServer Class
   * Does most of the work. Individual Die Servers just need to implement
   * {@link #buildInternetRollString} and {@link #parseInternetRollString} methods.
   */

  public static abstract class DieServer {

    protected java.util.Random ran;
    protected String name;
    protected String description;
    protected boolean emailOnly;
    protected int maxRolls;
    protected int maxEmails;
    protected String serverURL;
    protected boolean passwdRequired = false;
    protected String password = "";
    protected boolean useEmail;
    protected String primaryEmail;
    protected String secondaryEmail;
    protected boolean canDoSeperateDice = false;

    /*
     * Each implemented die server must provide this routine to build a
     * string that will be sent to the internet site to drive the web-based
     * die server. This will usually be a control string passed toa cgi script
     * on the site.
     */
    public abstract String[] buildInternetRollString(RollSet mr);

    /*
     * Each implemented die server must provide this routine to interpret the
     * html output generated by the site in response to the buildInternetRollString
     * call.
     */
    public abstract void parseInternetRollString(RollSet rollSet, Vector results);

    /*
     * Internet Die Servers should always implement roll by calling back to
     * {@link #doInternetRoll}
     */
    public abstract void roll(RollSet mr, FormattedString format);

    public DieServer() {
      ran = GameModule.getGameModule().getRNG();
    }

    /*
     * Some Internet servers can only roll specific numbers of dice or
     * dice with specific sides. These are the default settings.
     */
    public int[] getnDiceList() {
      return new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20};
    }

    public int[] getnSideList() {
      return new int[]{2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 20, 30, 50, 100, 1000};
    }

    public String getName() {
      return name;
    }

    public String getDescription() {
      return description;
    }

    public boolean isPasswdRequired() {
      return passwdRequired;
    }

    public String getPasswd() {
      return password;
    }

    public void setPasswd(String s) {
      password = s;
    }

    public void setPrimaryEmail(String e) {
      primaryEmail = e;
    }

    public String getPrimaryEmail() {
      return primaryEmail;
    }

    public void setSecondaryEmail(String e) {
      secondaryEmail = e;
    }

    public String getSecondaryEmail() {
      return secondaryEmail;
    }

    public void setUseEmail(boolean use) {
      useEmail = use;
    }

    public boolean getUseEmail() {
      return useEmail;
    }

    public int getMaxEmails() {
      return maxEmails;
    }

    /**
     * The text reported before the results of the roll
     */
    protected String getReportPrefix(String d) {
      return " *** " + d + " = ";
    }

    /**
     * The text reported after the results of the roll;
     * @deprecated
     */
    protected String getReportSuffix() {
      return " ***  <" + GameModule.getGameModule().getChatter().getHandle() + ">";
    }

    /*
     * Called by the Inbuilt server - Basically the same as the code
     * in the original DiceButton
     */
    public void doInbuiltRoll(RollSet mroll) {
      DieRoll[] rolls = mroll.getDieRolls();
      for (int i = 0; i < rolls.length; i++) {
        DieRoll roll = rolls[i];
        String desc = roll.getDescription();
        int nSides = roll.getNumSides();
        int nDice = roll.getNumDice();
        int plus = roll.getPlus();
        boolean reportTotal = roll.isReportTotal();

        String val = getReportPrefix(desc);
        int total = 0;
        for (int j = 0; j < nDice; ++j) {
          int result = (int) (ran.nextFloat() * nSides + 1) + plus;
          if (reportTotal) {
            total += result;
          }
          else {
            val += result;
            if (j < nDice - 1)
              val += ",";
          }

          if (reportTotal)
            val += total;

          val += getReportSuffix();
          GameModule.getGameModule().getChatter().send(val);
        }
      }
    }

    /*
     * Internet Servers will call this routine to do their dirty work.
     */
    public void doInternetRoll(final RollSet mroll, final FormattedString format) {
      BackgroundTask task = new BackgroundTask() {
        private IOException error;

        public void doFirst() {
          try {
            doIRoll(mroll);
          }
          catch (IOException e) {
            error = e;
          }
        }

        public void doLater() {
          if (error == null) {
            reportResult(mroll, format);
          }
          else {
            String s = "- Internet dice roll attempt " + mroll.getDescription() + " failed.";
            GameModule.getGameModule().getChatter().send(s);
          }
        }
      };
      task.start();
    }

    /**
     * Use the configured FormattedString to format the result of a roll
     * @param result
     * @return
     */
    protected String formatResult(String description, String result, FormattedString format) {
      format.setProperty(DiceButton.RESULT, result);
      format.setProperty(InternetDiceButton.DETAILS, description);
      String text = format.getText();
      String report = text.startsWith("*") ? "*" + text : "* " + text;
      return report;
    }


    public void reportResult(RollSet mroll, FormattedString format) {
      DieRoll[] rolls = mroll.getDieRolls();
      for (int i = 0; i < rolls.length; i++) {
        DieRoll roll = rolls[i];
        int nDice = roll.getNumDice();
        boolean reportTotal = roll.isReportTotal();

        String val = "";
        int total = 0;

        for (int j = 0; j < nDice; j++) {
          int result = roll.getResult(j);
          if (reportTotal) {
            total += result;
          }
          else {
            val += result;
            if (j < nDice - 1)
              val += ",";
          }
        }

        if (reportTotal)
          val += total;

        val = formatResult(rolls[i].getDescription(), val, format);
        GameModule.getGameModule().getChatter().send(val);
      }
    }

    public void doIRoll(RollSet toss) throws IOException {

      String[] rollString = buildInternetRollString(toss);
      Vector returnString = new Vector();
      //            rollString[0] =
      //                "number1=2&type1=6&number2=2&type2=30&number3=2&type3=30"
      //                    + "&number4=0&type4=2&number5=0&type5=2&number6=0&type6=2&number7=0&type7=2"
      //                    + "&number8=0&type8=2&number9=0&type9=2&number10=0&type10=2"
      //                    + "&emails=&email=b.easton@uws.edu.au&password=IG42506&Submit=Throw+Dice";
      URL url = new URL(serverURL);

      URLConnection connection = url.openConnection();
      connection.setDoOutput(true);

      PrintWriter out = new PrintWriter(connection.getOutputStream());
      for (int i = 0; i < rollString.length; i++) {
        out.println(rollString[i]);
      }

      out.close();

      BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));

      String inputLine;
      int i = 0;

      while ((inputLine = in.readLine()) != null)
        returnString.add(i++, inputLine);

      in.close();

      parseInternetRollString(toss, returnString);

    }

    /**
     *
     * Extract the portion of the email address withing the  angle brackets.
     * Allows Email addresses like 'Joe Blow <j.blow@somewhere.com>'
     */
    public String extractEmail(String email) {
      int start = email.indexOf("<");
      int end = email.indexOf(">");
      if (start >= 0 && end >= 0 && end > start) {
        return email.substring(start + 1, end);
      }
      else {
        return email;
      }
    }

  }

  /**
   *
   * ShadowDice Dice Server
   *
   */
  private class ShadowDiceDieServer extends DieServer {

    public ShadowDiceDieServer() {

      name = "ShadowDice";
      description = "ShadowDice Dice Server";
      emailOnly = false;
      maxRolls = 0;
      maxEmails = 0;
      serverURL = "http://www.gamerz.net/shadowdice/shadowdice.cgi";
      passwdRequired = false;
      canDoSeperateDice = true;
    }

    public String[] buildInternetRollString(RollSet toss) {

      final String CRLF = "%0D%0A"; // CRLF
      final String LSQUARE = "%5B"; // '['
      final String RSQUARE = "%5D"; // ']'
      final String HASH = "%23";
//      final String PLUS = "%2B";

      String desc, s, pEmail = "", sEmail = "";

      if (getUseEmail()) {
        pEmail = extractEmail(getPrimaryEmail());
        sEmail = extractEmail(getSecondaryEmail());
      }

      desc = hexify(toss.description);

      s = "mto=" + pEmail + "&mcc=" + sEmail + "&yem=" + pEmail;
      s += "&sbj=" + desc;
      s += "&msg=" + ROLL_MARKER + CRLF + desc + CRLF;

      int mLen = toss.getMaxDescLength();

      DieRoll[] rolls = toss.getDieRolls();
      for (int i = 0; i < rolls.length; i++) {
        s += hexify(rolls[i].getDescription());
        for (int j = 0; j < mLen - rolls[i].getDescription().length(); j++) {
          s += ' ';
        }
        s += ' ' + HASH;
        int nd = rolls[i].getNumDice();
        int ns = rolls[i].getNumSides();
//        int p = rolls[i].getPlus();
        for (int j = 0; j < nd; j++) {
          s += LSQUARE + "1d" + ns + RSQUARE;
        }
        s += CRLF;
      }

      s += "&todo=Action%21&hid=1";
      s = s.replace(' ', '+'); // No spaces allowed, use '+' instead.

      return new String[]{s};
    }

    /*
     * The Irony server requires most of the non-alphanumerics to be
     * converted to a hex escape code %nn. '*-_.' excepted.
     * '#' characters interfere with the output parsing and are stripped out.
     */
    public String hexify(String s) {

      final String hexyChars = "~!$%^&()+`={}[]|:;'<>,?/\\\"";
      StringBuffer b = new StringBuffer();

      for (int i = 0; i < s.length(); i++) {
        char c = s.charAt(i);

        if (c == '#') {
          b.append('.');
        }
        else if (hexyChars.indexOf(c) >= 0) {
          b.append("%" + Integer.toHexString(c).toUpperCase());
        }
        else {
          b.append(c);
        }
      }
      return b.toString();
    }

    public void parseInternetRollString(RollSet rollSet, Vector results) {

      Enumeration e = results.elements();

      // Initialise and search for start line
      String line = (String) e.nextElement();
      while (e.hasMoreElements() && !line.startsWith("! " + ROLL_MARKER))
        line = (String) e.nextElement();

      // Skip description line
      line = (String) e.nextElement();

      // And process the results, 1 per roll in the multiroll
      DieRoll[] rolls = rollSet.getDieRolls();
      for (int i = 0; i < rolls.length; i++) {

        line = (String) e.nextElement();

        int firsthash = line.indexOf("#") - 1;
        StringTokenizer st = new StringTokenizer(line.substring(firsthash), " ");

        for (int j = 0; j < rollSet.dieRolls[i].getNumDice(); j++) {
          st.nextToken();
          String result = st.nextToken();
          int res = Integer.parseInt(result);
          rollSet.dieRolls[i].setResult(j, res);
        }
      }
    }

    public void roll(RollSet mr, FormattedString format) {
      super.doInternetRoll(mr, format);
    }

  }

}