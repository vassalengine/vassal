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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import javax.swing.BoxLayout;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
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
import VASSAL.i18n.Resources;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.SequenceEncoder;

/**
 * Maintains a list of players involved in the current game
 */
// I18n: Complete
public class PlayerRoster implements Configurable, CommandEncoder, GameComponent {
  public static final String BUTTON_ICON = "buttonIcon";
  public static final String BUTTON_TEXT = "buttonText";
  public static final String TOOL_TIP = "buttonToolTip";
  public static final String COMMAND_PREFIX = "PLAYER\t";
  protected List players = new ArrayList();
  protected List sides = new ArrayList();
  protected LaunchButton retireButton;
  protected List sideChangeListeners = new ArrayList();

  public PlayerRoster() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    };
    retireButton = new LaunchButton(Resources.getString("PlayerRoster.retire"), TOOL_TIP, BUTTON_TEXT, null, BUTTON_ICON, al);
    retireButton.setToolTipText(Resources.getString("PlayerRoster.allow_another"));
    retireButton.setVisible(false);
  }

  public void removeFrom(Buildable parent) {
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    GameModule.getGameModule().removeCommandEncoder(this);
  }

  public void remove(Buildable child) {
  }

  public void build(Element e) {
    if (e != null) {
      NamedNodeMap attributes = e.getAttributes();
      for (int i = 0; i < attributes.getLength(); ++i) {
        Attr att = (Attr) attributes.item(i);
        retireButton.setAttribute(att.getName(), att.getValue());
      }
      NodeList n = e.getElementsByTagName("*");
      sides.clear();
      for (int i = 0; i < n.getLength(); ++i) {
        Element el = (Element) n.item(i);
        sides.add(Builder.getText(el));
      }
    }
  }

  public String getConfigureName() {
    return null;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.PlayerRoster.component_type");
  }

  public void add(Buildable child) {
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public Element getBuildElement(Document doc) {
    Element el = doc.createElement(getClass().getName());
    String att = retireButton.getAttributeValueString(BUTTON_TEXT);
	if (att != null) el.setAttribute(BUTTON_TEXT, att);
    att = retireButton.getAttributeValueString(BUTTON_ICON);
	if (att != null) el.setAttribute(BUTTON_ICON, att);
	att = retireButton.getAttributeValueString(TOOL_TIP);
    if (att != null) el.setAttribute(TOOL_TIP, att);
    for (Iterator e = sides.iterator(); e.hasNext();) {
      Element sub = doc.createElement("entry");
      sub.appendChild(doc.createTextNode((String) e.next()));
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
    return HelpFile.getReferenceManualPage("GameModule.htm", "Definition_of_Player_Sides");
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable b) {
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getToolBar().add(retireButton);
  }

  protected void launch() {
    String mySide = getMySide();
    if (mySide != null || sides.size() != players.size()) {
      String[] options = sides.size() == players.size() ? new String[] {Resources.getString(Resources.YES), Resources.getString(Resources.NO)} : new String[] {Resources.getString("PlayerRoster.become_observer"), Resources.getString("PlayerRoster.join_another_side"), Resources.getString(Resources.CANCEL)};
      final int CANCEL = options.length - 1;
      int option = (JOptionPane.showOptionDialog(GameModule.getGameModule().getFrame(), Resources.getString("PlayerRoster.give_up_position", mySide), Resources.getString("PlayerRoster.retire"),
          JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, Resources.getString("PlayerRoster.become_observer")));
      if (option == 0) {
        String oldSide = getMySide();
        remove(GameModule.getUserId());
        String newSide = getMySide();
        fireSideChange(oldSide, newSide);
      }
      else if (option != CANCEL) {
        String oldSide = getMySide();
        remove(GameModule.getUserId());
        promptForSide();
        String newSide = getMySide();
        fireSideChange(oldSide, newSide);
      }
    }
  }

  protected void fireSideChange(String oldSide, String newSide) {
    for (Iterator it = sideChangeListeners.iterator(); it.hasNext();) {
      SideChangeListener l = (SideChangeListener) it.next();
      l.sideChanged(oldSide,newSide);
    }
  }

  public static boolean isActive() {
    return getInstance() != null;
  }
  
  protected static PlayerRoster getInstance() {
    PlayerRoster r = null;
    Enumeration e = GameModule.getGameModule().getComponents(PlayerRoster.class);
    if (e.hasMoreElements()) {
      r = (PlayerRoster) e.nextElement();
    }
    return r;
  }

  public static String getMySide() {
    PlayerRoster r = getInstance();
    if (r != null) {
      Entry[] players = r.getPlayers();
      for (int i = 0; i < players.length; ++i) {
        if (players[i].playerId.equals(GameModule.getUserId())) {
          return players[i].side;
        }
      }
    }
    return null;
  }

  public Entry[] getPlayers() {
    Entry[] p = new Entry[players.size()];
    for (int i = 0; i < p.length; ++i) {
      p[i] = (Entry) players.get(i);
    }
    return p;
  }

  public void add(String playerId, String playerName, String side) {
    Entry e = new Entry(playerId, playerName, side);
    if (players.contains(e)) {
      players.set(players.indexOf(e), e);
    }
    else {
      players.add(e);
    }
  }

  public void remove(String playerId) {
    Entry e = new Entry(playerId, null, null);
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
      SequenceEncoder se = new SequenceEncoder('\t');
      se.append(((Add) c).id);
      se.append(((Add) c).name);
      se.append(((Add) c).side);
      return COMMAND_PREFIX + se.getValue();
    }
    else {
      return null;
    }
  }

  public Command getRestoreCommand() {
    Command c = null;
    for (Iterator e = players.iterator(); e.hasNext();) {
      Entry entry = (Entry) e.next();
      Command sub = new Add(this, entry.playerId, entry.playerName, entry.side);
      c = c == null ? sub : c.append(sub);
    }
    return c;
  }

  public void setup(boolean gameStarting) {
    if (gameStarting) {
      Entry me = new Entry(GameModule.getUserId(), GlobalOptions.getInstance().getPlayerId(), null);
      if (players.contains(me)) {
        Entry saved = (Entry) players.get(players.indexOf(me));
        saved.playerName = me.playerName;
      }
      else if (sides.size() > 0){
        promptForSide();
      }
    }
    else {
      players.clear();
    }
    retireButton.setVisible(gameStarting && getMySide() != null);
  }

  protected void promptForSide() {
    List availableSides = new ArrayList(sides);
    List alreadyTaken = new ArrayList();
    for (int i = 0; i < players.size(); ++i) {
      alreadyTaken.add(((Entry) players.get(i)).side);
    }
    availableSides.removeAll(alreadyTaken);
    availableSides.add(0, OBSERVER);
    String newSide = (String) JOptionPane.showInputDialog(GameModule.getGameModule().getFrame(), Resources.getString("PlayerRoster.join_game_as"), Resources.getString("PlayerRoster.choose_side"),
        JOptionPane.QUESTION_MESSAGE, null, (String[]) availableSides.toArray(new String[availableSides.size()]), OBSERVER);
    if (newSide != null && !OBSERVER.equals(newSide)) {
      Entry me = new Entry(GameModule.getUserId(), GlobalOptions.getInstance().getPlayerId(), newSide);
      Add a = new Add(this, me.playerId, me.playerName, me.side);
      a.execute();
      GameModule.getGameModule().getServer().sendToOthers(a);
    }
  }

  public static class Entry {
    public String playerId;
    public String playerName;
    public String side;

    public Entry(String id, String name, String side) {
      playerId = id;
      playerName = name;
      this.side = side;
    }

    public boolean equals(Object o) {
      if (o instanceof Entry && playerId != null) {
        return playerId.equals(((Entry) o).playerId);
      }
      else {
        return false;
      }
    }
  }

  public static class Add extends Command {
    private PlayerRoster roster;
    private String id, name, side;

    public Add(PlayerRoster r, String playerId, String playerName, String side) {
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

      sidesConfig = new StringArrayConfigurer(null, "Sides available to players", (String[]) sides.toArray(new String[sides.size()]));
      sidesConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          sides.clear();
          sides.addAll(Arrays.asList(sidesConfig.getStringArray()));
        }
      });
      controls.add(sidesConfig.getControls());

      textConfig = new StringConfigurer(BUTTON_TEXT, "'Retire' button text:  ", retireButton.getAttributeValueString(BUTTON_TEXT));
      textConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          retireButton.setAttribute(BUTTON_TEXT, textConfig.getValueString());
        }
      });
      controls.add(textConfig.getControls());

      tooltipConfig = new StringConfigurer(TOOL_TIP, "'Retire' button tooltip:  ", retireButton.getAttributeValueString(TOOL_TIP));
      tooltipConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          retireButton.setAttribute(TOOL_TIP, tooltipConfig.getValueString());
        }
      });
      controls.add(tooltipConfig.getControls());
      
      iconConfig = new IconConfigurer(BUTTON_ICON, "'Retire' button icon:  ", null);
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

  private static String OBSERVER = Resources.getString("PlayerRoster.observer");

}
