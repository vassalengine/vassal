/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.build.module;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.Configurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.SequenceEncoder;

/**
 * Maintains a list of players involved in the current game
 */
public class PlayerRoster extends AbstractConfigurable implements CommandEncoder, GameComponent, GameSetupStep {
  public static final String BUTTON_ICON = "buttonIcon"; //$NON-NLS-1$
  public static final String BUTTON_TEXT = "buttonText"; //$NON-NLS-1$
  public static final String TOOL_TIP = "buttonToolTip"; //$NON-NLS-1$
  public static final String SIDES = "sides"; //$NON-NLS-1$
  public static final String COMMAND_PREFIX = "PLAYER\t"; //$NON-NLS-1$
  public static final String OBSERVER = "<observer>"; //$NON-NLS-1$
  protected List<PlayerInfo> players = new ArrayList<PlayerInfo>();
  protected List<String> sides = new ArrayList<String>();
  protected String[] untranslatedSides;
  protected LaunchButton retireButton;
  protected List<SideChangeListener> sideChangeListeners =
    new ArrayList<SideChangeListener>();

  protected String translatedObserver;

  private boolean pickedSide = false;

  public PlayerRoster() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    };
    retireButton = new LaunchButton(Resources.getString("PlayerRoster.retire"), TOOL_TIP, BUTTON_TEXT, null, BUTTON_ICON, al); //$NON-NLS-1$
    retireButton.setToolTipText(Resources.getString("PlayerRoster.allow_another")); //$NON-NLS-1$
    retireButton.setVisible(false);

    translatedObserver = Resources.getString("PlayerRoster.observer"); //$NON-NLS-1$
  }

  public void removeFrom(Buildable parent) {
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    GameModule.getGameModule().removeCommandEncoder(this);
  }

  public void remove(Buildable child) {
  }

  public void build(Element e) {
    if (e != null) {
      final NamedNodeMap attributes = e.getAttributes();
      for (int i = 0; i < attributes.getLength(); ++i) {
        final Attr att = (Attr) attributes.item(i);

        // Old versions of VASSAL (pre-2.9?) wrote "Retire" as the
        // icon filename for the retireButton, even when no such
        // image existed in the archive. This test blocks irritating
        // errors due to nonexistent "Retire" images, by ignoring
        // the buttonIcon attribute when the value is "Retire" but
        // no such image can be found in the archive.
        if ("buttonIcon".equals(att.getName()) && //$NON-NLS-1$
            "Retire".equals(att.getValue())) { //$NON-NLS-1$
          try {
            GameModule.getGameModule()
                      .getDataArchive()
                      .getInputStream(DataArchive.IMAGE_DIR + att.getValue());
          }
          catch (IOException ex) {
            continue;
          }
        }

        retireButton.setAttribute(att.getName(), att.getValue());
        Localization.getInstance()
                    .saveTranslatableAttribute(this, att.getName(),
                                                     att.getValue());
      }

      final NodeList n = e.getElementsByTagName("*"); //$NON-NLS-1$
      sides.clear();
      for (int i = 0; i < n.getLength(); ++i) {
        final Element el = (Element) n.item(i);
        sides.add(Builder.getText(el));
      }
      Localization.getInstance()
                  .saveTranslatableAttribute(this, SIDES, getSidesAsString());
    }
  }

  public String getConfigureName() {
    return null;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.PlayerRoster.component_type"); //$NON-NLS-1$
  }

  public void add(Buildable child) {
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public Element getBuildElement(Document doc) {
    Element el = doc.createElement(getClass().getName());
    String att = retireButton.getAttributeValueString(BUTTON_TEXT);
    if (att != null)
      el.setAttribute(BUTTON_TEXT, att);
    att = retireButton.getAttributeValueString(BUTTON_ICON);
    if (att != null)
      el.setAttribute(BUTTON_ICON, att);
    att = retireButton.getAttributeValueString(TOOL_TIP);
    if (att != null)
      el.setAttribute(TOOL_TIP, att);
    for (String s : sides) {
      Element sub = doc.createElement("entry"); //$NON-NLS-1$
      sub.appendChild(doc.createTextNode(s));
      el.appendChild(sub);
    }
    return el;
  }

  public Configurer getConfigurer() {
    return new Con();
  }

  public void addPropertyChangeListener(PropertyChangeListener l) {
  }

  public static void addSideChangeListener(SideChangeListener l) {
    PlayerRoster r = getInstance();
    if (r != null) {
      r.sideChangeListeners.add(l);
    }
  }

  public static void removeSideChangeListener(SideChangeListener l) {
    PlayerRoster r = getInstance();
    if (r != null) {
      r.sideChangeListeners.remove(l);
    }
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.htm", "Definition_of_Player_Sides"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public void addTo(Buildable b) {
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().getGameState().addGameSetupStep(this);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getToolBar().add(retireButton);
  }

  protected void launch() {
    final String mySide = getMySide();
    if (mySide == null && allSidesAllocated()) {
      return;
    }

    final String[] options = allSidesAllocated() ?
      new String[]{
        Resources.getString(Resources.YES),
        Resources.getString(Resources.NO)
      } :
      new String[]{
        Resources.getString("PlayerRoster.become_observer"), //$NON-NLS-1$
        Resources.getString("PlayerRoster.join_another_side"), //$NON-NLS-1$
        Resources.getString(Resources.CANCEL)
      };

    final int CANCEL = options.length - 1;

    final int option = JOptionPane.showOptionDialog(
      GameModule.getGameModule().getFrame(),
      Resources.getString("PlayerRoster.give_up_position", getMyLocalizedSide()),
      Resources.getString("PlayerRoster.retire"), //$NON-NLS-1$
      JOptionPane.YES_NO_OPTION,
      JOptionPane.QUESTION_MESSAGE,
      null,
      options,
      Resources.getString("PlayerRoster.become_observer") //$NON-NLS-1$
    );

    if (option != CANCEL) {
      final String oldSide = getMySide();

      String newSide;
      if (option == 0) {
        newSide = OBSERVER;
      }
      else {
        newSide = promptForSide();
        if (newSide == null) {
          return;
        }
      }

      remove(GameModule.getUserId());

      final PlayerInfo me = new PlayerInfo(
        GameModule.getUserId(),
        GlobalOptions.getInstance().getPlayerId(),
        newSide
      );
      final Add a = new Add(this, me.playerId, me.playerName, me.side);
      a.execute();
      GameModule.getGameModule().getServer().sendToOthers(a);

      newSide = getMySide();
      fireSideChange(oldSide, newSide);
    }
  }

  protected void fireSideChange(String oldSide, String newSide) {
    for (SideChangeListener l : sideChangeListeners) {
      l.sideChanged(oldSide, newSide);
    }
  }

  public static boolean isActive() {
    return getInstance() != null;
  }

  protected static PlayerRoster getInstance() {
    for (PlayerRoster pr :
         GameModule.getGameModule().getComponentsOf(PlayerRoster.class)) {
      return pr;
    }
    return null;
  }

  public static String getMySide() {
    return getMySide(false);
  }

  public static String getMyLocalizedSide() {
    return getMySide(true);
  }

  protected static String getMySide(boolean localized) {
    final PlayerRoster r = getInstance();
    if (r != null) {
      for (PlayerInfo pi : r.getPlayers()) {
        if (pi.playerId.equals(GameModule.getUserId())) {
          return localized ? pi.getLocalizedSide() : pi.getSide();
        }
      }
    }
    return null;
  }

  public PlayerInfo[] getPlayers() {
    return players.toArray(new PlayerInfo[players.size()]);
  }

  public void add(String playerId, String playerName, String side) {
    PlayerInfo e = new PlayerInfo(playerId, playerName, side);
    if (players.contains(e)) {
      players.set(players.indexOf(e), e);
    }
    else {
      players.add(e);
    }
  }

  public void remove(String playerId) {
    PlayerInfo e = new PlayerInfo(playerId, null, null);
    players.remove(e);
  }

  public Command decode(String command) {
    if (command.startsWith(COMMAND_PREFIX)) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, '\t');
      st.nextToken();
      return new Add(this, st.nextToken(), st.nextToken(), st.nextToken());
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    if (c instanceof Add) {
      final Add a = (Add) c;
      final SequenceEncoder se = new SequenceEncoder('\t');
      se.append(a.id).append(a.name).append(a.side);
      return COMMAND_PREFIX + se.getValue();
    }
    else {
      return null;
    }
  }

  public Command getRestoreCommand() {
    Command c = null;
    for (PlayerInfo entry : players) {
      Command sub = new Add(this, entry.playerId, entry.playerName, entry.side);
      c = c == null ? sub : c.append(sub);
    }
    return c;
  }

  public void setup(boolean gameStarting) {
    if (gameStarting) {
      PlayerInfo me = new PlayerInfo(GameModule.getUserId(),
        GlobalOptions.getInstance().getPlayerId(), null);
      if (players.contains(me)) {
        PlayerInfo saved = players.get(players.indexOf(me));
        saved.playerName = me.playerName;
      }
    }
    else {
      players.clear();
    }
    retireButton.setVisible(gameStarting && getMySide() != null);
    pickedSide = false;
  }

  public void finish() {
    final String newSide = untranslateSide(sideConfig.getValueString());
    if (newSide != null) {
      Add a = new Add(this, GameModule.getUserId(), GlobalOptions.getInstance().getPlayerId(), newSide);
      a.execute();
      GameModule.getGameModule().getServer().sendToOthers(a);
    }
    retireButton.setVisible(getMySide() != null);
    pickedSide = true;
  }

  public Component getControls() {
    ArrayList<String> availableSides = new ArrayList<String>(sides);
    ArrayList<String> alreadyTaken = new ArrayList<String>();

    for (PlayerInfo p : players) {
      alreadyTaken.add(p.side);
    }

    availableSides.removeAll(alreadyTaken);
    availableSides.add(0, translatedObserver);
    sideConfig = new StringEnumConfigurer(null,
      Resources.getString("PlayerRoster.join_game_as"), //$NON-NLS-1$
      availableSides.toArray(new String[availableSides.size()]));
    sideConfig.setValue(translatedObserver);
    return sideConfig.getControls();
  }

  public String getStepTitle() {
    return Resources.getString("PlayerRoster.choose_side"); //$NON-NLS-1$
  }

  // Implement GameSetupStep
  public boolean isFinished() {
    if (pickedSide) {
      return true;
    }

    // Step is always finished if all sides are allocated
    if (allSidesAllocated()) {
      return true;
    }

    // If we are already recorded as a player (i.e. in Saved Game), then
    // the step is only finished if we are not the Observer.
    final PlayerInfo newPlayerInfo = new PlayerInfo(
      GameModule.getUserId(),
      GlobalOptions.getInstance().getPlayerId(), null
    );

    final int i = players.indexOf(newPlayerInfo);
    if (i != -1) {
      return !OBSERVER.equals(players.get(i).getSide());
    }

    // Step is not finished
    return false;
  }

  /**
   *
   * @return true if all sides have been claimed by a player
   */
  protected boolean allSidesAllocated() {
    int allocatedSideCount = 0;
    for (PlayerInfo p : players) {
      if (!OBSERVER.equals(p.getSide())) {
        ++allocatedSideCount;
      }
    }
    return sides.size() == allocatedSideCount;
  }

  protected String promptForSide() {
    ArrayList<String> availableSides = new ArrayList<String>(sides);
    ArrayList<String> alreadyTaken = new ArrayList<String>();

    for (PlayerInfo p : players) {
      alreadyTaken.add(p.side);
    }

    availableSides.removeAll(alreadyTaken);
    availableSides.add(0, translatedObserver);

    final GameModule g = GameModule.getGameModule();
    String newSide = (String) JOptionPane.showInputDialog(
      g.getFrame(),
      Resources.getString("PlayerRoster.join_game_as"), //$NON-NLS-1$
      Resources.getString("PlayerRoster.choose_side"), //$NON-NLS-1$
      JOptionPane.QUESTION_MESSAGE,
      null,
      availableSides.toArray(new String[availableSides.size()]),
      translatedObserver
    );

    // OBSERVER must always be stored internally in English.
    if (translatedObserver.equals(newSide)) {
      newSide = OBSERVER;
    }
    return newSide;
/*
    if (newSide != null) {
      final PlayerInfo me = new PlayerInfo(GameModule.getUserId(), GlobalOptions.getInstance().getPlayerId(), newSide);
      final Add a = new Add(this, me.playerId, me.playerName, me.side);
      a.execute();
      g.getServer().sendToOthers(a);
    }
*/
  }

  public static class PlayerInfo {
    public String playerId;
    public String playerName;
    private String side;

    public PlayerInfo(String id, String name, String side) {
      if (id == null) {
        throw new NullPointerException("Player id cannot be null"); //$NON-NLS-1$
      }
      playerId = id;
      playerName = name;
      this.side = side;
    }

    public boolean equals(Object o) {
      if (o instanceof PlayerInfo && playerId != null) {
        return playerId.equals(((PlayerInfo) o).playerId);
      }
      else {
        return false;
      }
    }

    public String getSide() {
      return side;
    }

    public String getLocalizedSide() {
      return PlayerRoster.getInstance().translateSide(side);
    }
  }

  public static class Add extends Command {
    private PlayerRoster roster;
    private String id, name, side;

    public Add(PlayerRoster r, String playerId,
               String playerName, String side) {
      roster = r;
      id = playerId;
      name = playerName;
      this.side = side;
    }

    protected void executeCommand() {
      roster.add(id, name, side);
    }

    protected Command myUndoCommand() {
      return null;
    }
  }

  private class Con extends Configurer {
    private StringArrayConfigurer sidesConfig;
    private IconConfigurer iconConfig;
    private StringConfigurer textConfig;
    private StringConfigurer tooltipConfig;
    private JPanel controls;

    private Con() {
      super(null, null);
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
      sidesConfig = new StringArrayConfigurer(null, Resources.getString("Editor.PlayerRoster.sides_available"), sides.toArray(new String[sides.size()])); //$NON-NLS-1$
      sidesConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          sides.clear();
          sides.addAll(Arrays.asList(sidesConfig.getStringArray()));
        }
      });
      controls.add(sidesConfig.getControls());
      textConfig = new StringConfigurer(BUTTON_TEXT, Resources.getString("Editor.PlayerRoster.retire_button_text"), retireButton.getAttributeValueString(BUTTON_TEXT)); //$NON-NLS-1$
      textConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          retireButton.setAttribute(BUTTON_TEXT, textConfig.getValueString());
        }
      });
      controls.add(textConfig.getControls());
      tooltipConfig = new StringConfigurer(TOOL_TIP, Resources.getString("Editor.PlayerRoster.retire_button_tooltip"), retireButton.getAttributeValueString(TOOL_TIP)); //$NON-NLS-1$
      tooltipConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          retireButton.setAttribute(TOOL_TIP, tooltipConfig.getValueString());
        }
      });
      controls.add(tooltipConfig.getControls());
      iconConfig = new IconConfigurer(BUTTON_ICON, Resources.getString("Editor.PlayerRoster.retire_button_icon"), null); //$NON-NLS-1$
      iconConfig.setValue(retireButton.getIcon());
      iconConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          retireButton.setAttribute(BUTTON_ICON, iconConfig.getValueString());
        }
      });
      controls.add(iconConfig.getControls());
    }

    public String getValueString() {
      return null;
    }

    public void setValue(String s) {
    }

    public Component getControls() {
      return controls;
    }
  }

  /** Call-back interface for when a player changes sides during a game */
  public static interface SideChangeListener {
    void sideChanged(String oldSide, String newSide);
  }

  protected StringEnumConfigurer sideConfig;

  /**
   * PlayerRoster is not a true AbstractConfigurable, it handles
   * it's own configuration. Implement the rest of the AbstractConfigurable
   * abstract classes for i18n.
   */
  public String[] getAttributeNames() {
    return new String[] {
      BUTTON_TEXT,
      TOOL_TIP,
      SIDES
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      String.class,
      String.class
    };
  }

  public String getAttributeValueString(String key) {
    if (SIDES.equals(key)) {
      return getSidesAsString();
    }
    return retireButton.getAttributeValueString(key);
  }

  /*
   * Only ever called from Language.translate()
   */
  public void setAttribute(String key, Object value) {
    if (SIDES.equals(key)) {
      untranslatedSides = sides.toArray(new String[sides.size()]);
      String[] s = StringArrayConfigurer.stringToArray((String) value);
      sides = new ArrayList<String>(s.length);
      for (int i = 0; i < s.length; i++) {
        sides.add(s[i]);
      }
    }
    else {
      retireButton.setAttribute(key, value);
    }
  }

  protected String getSidesAsString() {
    String[] s = sides.toArray(new String[sides.size()]);
    return StringArrayConfigurer.arrayToString(s);
  }

  protected String untranslateSide(String side) {
    if (translatedObserver.equals(side)) {
      return OBSERVER;
    }
    if (untranslatedSides != null) {
      for (int i = 0; i < sides.size(); i++) {
        if (sides.get(i).equals(side)) {
          return untranslatedSides[i];
        }
      }
    }
    return side;
  }

  protected String translateSide(String side) {
    if (OBSERVER.equals(side)) {
      return translatedObserver;
    }
    if (untranslatedSides != null) {
      for (int i = 0; i < untranslatedSides.length; i++) {
        if (untranslatedSides[i].equals(side)) {
          return sides.get(i);
        }
      }
    }
    return side;
  }

  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString("Editor.button_text_label"),
      Resources.getString("Editor.tooltip_text_label"),
      Resources.getString("Editor.PlayerRoster.sides_label")
    };
  }
}
