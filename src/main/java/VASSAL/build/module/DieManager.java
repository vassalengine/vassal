/*
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.DefaultListModel;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.dice.BonesDiceServer;
import VASSAL.build.module.dice.DieServer;
import VASSAL.build.module.dice.RollSet;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.FormattedString;

/**
 * @author Brent Easton
 *
 * Internet Die Roller Manager. Includes all the smarts to interface to web-based
 * Die Servers
 */

public class DieManager extends AbstractConfigurable {

  private Map<String,DieServer> servers;
  private List<InternetDiceButton> dieButtons = new ArrayList<>();
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

  public static final String DESC = "description";
  public static final String DFLT_NSIDES = "dfltnsides";
  public static final String DFLT_NDICE = "dfltndice";

  public DieManager() {

    DieServer d;
    servers = new HashMap<>();

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

//    d = new ShadowDiceDieServer();
//    servers.put(d.getName(), d);

    d = new BonesDiceServer();
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

    DefaultListModel<String> m = abook.getModel();
    ListDataListener ldl = new ListDataListener() {

      @Override
      public void contentsChanged(ListDataEvent arg0) {
        setSemailValues();
      }

      @Override
      public void intervalAdded(ListDataEvent arg0) {
        setSemailValues();
      }

      @Override
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
// FIXME: better to return zero-length array
    if (servers == null) {
      return null;
    }
    else {
      return servers.keySet().toArray(new String[0]);
    }
  }

  // Return descriptions of all known dice servers
  public String[] getDescriptions() {
// FIXME: better to return zero-length array
    if (servers == null) {
      return null;
    }
    else {
      String[] s = new String[servers.size()];
      int i = 0;
      for (DieServer d : servers.values()) {
        s[i++] = d.getDescription();
      }
      return s;
    }
  }

  // Return server matching Name
  public DieServer getServerForName(String name) {
    return servers.get(name);
  }

  // Return server matching Description
  public DieServer getServerFromDescription(String de) {
    for (DieServer d : servers.values()) {
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
    server.setUseEmail((Boolean) prefs.getValue(USE_EMAIL));
    server.setPrimaryEmail((String) prefs.getValue(PRIMARY_EMAIL));
    server.setSecondaryEmail((String) prefs.getValue(SECONDARY_EMAIL));

    useMultiRoll = (Boolean) prefs.getValue(MULTI_ROLL);

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

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
        Resources.getString("Editor.DieManager.description"), //$NON-NLS-1$
        Resources.getString("Editor.DieManager.ndice"), //$NON-NLS-1$
        Resources.getString("Editor.DieManager.nsides") //$NON-NLS-1$
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      Integer.class,
      Integer.class
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      DESC,
      DFLT_NDICE,
      DFLT_NSIDES
    };
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (DESC.equals(key)) {
      desc = (String) value;
    }
    else if (DFLT_NDICE.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      defaultNDice = (Integer) value;
    }
    else if (DFLT_NSIDES.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      defaultNSides = (Integer) value;
    }
  }

  @Override
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

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{InternetDiceButton.class};
  }

  @Override
  public void addTo(Buildable parent) {
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.DieManager.component_type"); //$NON-NLS-1$
  }

  public void setSecondaryEmail(String email) {
    GameModule.getGameModule().getPrefs().setValue(SECONDARY_EMAIL, email);
    server.setSecondaryEmail(email);
  }
}
