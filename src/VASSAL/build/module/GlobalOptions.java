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

import java.awt.Container;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;
import javax.swing.JOptionPane;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import VASSAL.Info;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.SingleChildInstance;
import VASSAL.configure.StringEnum;
import VASSAL.i18n.Resources;
import VASSAL.preferences.BooleanPreference;
import VASSAL.preferences.DoublePreference;
import VASSAL.preferences.EnumPreference;
import VASSAL.preferences.IntegerPreference;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.StringPreference;
import VASSAL.preferences.TextPreference;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.FormattedString;
import VASSAL.tools.IOUtils;

public class GlobalOptions extends AbstractConfigurable {
  public static final String NON_OWNER_UNMASKABLE = "nonOwnerUnmaskable"; //$NON-NLS-1$
  public static final String PROMPT_STRING = "promptString"; //$NON-NLS-1$
  public static final String CENTER_ON_MOVE = "centerOnMove"; //$NON-NLS-1$
  public static final String MARK_MOVED = "markMoved"; //$NON-NLS-1$
  public static final String AUTO_REPORT = "autoReport"; //$NON-NLS-1$
  public static final String ALWAYS = "Always";
  public static final String NEVER = "Never";
  public static final String PROMPT = "Use Preferences Setting";
  public static final String SINGLE_WINDOW = "singleWindow"; //$NON-NLS-1$
  public static final String MAXIMUM_HEAP = "maximumHeap"; //$NON-NLS-1$
  public static final String INITIAL_HEAP = "initialHeap"; //$NON-NLS-1$

  public static final String PLAYER_NAME = "playerName"; //$NON-NLS-1$
  public static final String PLAYER_SIDE = "playerSide"; //$NON-NLS-1$
  public static final String PLAYER_ID = "playerId"; //$NON-NLS-1$
  public static final String PLAYER_ID_FORMAT = "playerIdFormat"; //$NON-NLS-1$

  private String promptString = "Opponents can unmask my pieces";
  private String nonOwnerUnmaskable = NEVER;
  private String centerOnMoves = ALWAYS;
  private String autoReport = NEVER;
  private String markMoved = NEVER;

  private Map<String,Object> properties = new HashMap<String,Object>();
  private Map<String,Configurer> optionConfigurers =
    new LinkedHashMap<String,Configurer>();
  private Properties optionInitialValues = new Properties();

  private FormattedString playerIdFormat = new FormattedString("$" + PLAYER_NAME + "$"); //$NON-NLS-1$ //$NON-NLS-2$

  private static GlobalOptions instance = new GlobalOptions();
  private boolean useSingleWindow;

  public void addTo(Buildable parent) {
    instance = this;

    // should this moudule use a combined main window?
    final BooleanConfigurer combConf = new BooleanConfigurer(
      SINGLE_WINDOW,
      Resources.getString("GlobalOptions.use_combined"),  //$NON-NLS-1$
      Boolean.TRUE);
    GameModule.getGameModule().getPrefs().addOption(combConf);
    useSingleWindow = !Boolean.FALSE.equals(combConf.getValue());

    // the initial heap size for this module
    final IntConfigurer initHeapConf = new IntConfigurer(
      INITIAL_HEAP,
      Resources.getString("GlobalOptions.initial_heap"),  //$NON-NLS-1$
      Integer.valueOf(256));
    GameModule.getGameModule().getPrefs().addOption(initHeapConf);

    // the maximum heap size for this module
    final IntConfigurer maxHeapConf = new IntConfigurer(
      MAXIMUM_HEAP,
      Resources.getString("GlobalOptions.maximum_heap"),  //$NON-NLS-1$
      Integer.valueOf(512));
    GameModule.getGameModule().getPrefs().addOption(maxHeapConf);

    validator = new SingleChildInstance(GameModule.getGameModule(), getClass());
  }

  public static GlobalOptions getInstance() {
    return instance;
  }

  public boolean isUseSingleWindow() {
    return useSingleWindow;
  }

  @Deprecated
  public boolean isAveragedScaling() {
    return true;
  }

  public static String getConfigureTypeName() {
    return "Global Options"; //$NON-NLS-1$
  }

  public static class Prompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, NEVER, PROMPT};
    }
  }

  public static class PlayerIdFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{PLAYER_NAME, PLAYER_SIDE});
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] {StringPreference.class, 
        TextPreference.class,
        EnumPreference.class,
        IntegerPreference.class, 
        DoublePreference.class, 
        BooleanPreference.class} ;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Allow non-owners to unmask pieces:  ",
                        null,
                        "Center on opponent's moves:  ",
                        "Auto-report moves:  ",
                        "Player Id format:  "};
  }

  public String[] getAttributeNames() {
    ArrayList<String> attributes = new ArrayList<String>(Arrays.asList(
      new String[]{ NON_OWNER_UNMASKABLE,
                    PROMPT_STRING,
                    CENTER_ON_MOVE,
                    AUTO_REPORT,
                    PLAYER_ID_FORMAT }));

    for (String key : properties.keySet()) {
      attributes.add(key);
    }
    return attributes.toArray(new String[attributes.size()]);
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      Prompt.class,
      null,
      Prompt.class,
      Prompt.class,
      PlayerIdFormatConfig.class
    };
  }

  /**
   * Components may use GlobalOptions to store generic global attributes.
   * This method registers the given key as an attribute of the GlobalOptions with the given type.
   */
  public void addOption(Configurer option) {
    optionConfigurers.put(option.getKey(), option);
    Object initValue = optionInitialValues.get(option.getKey());
    if (initValue instanceof String) {
      option.setValue((String)initValue);
    }
    if (config != null) {
      ((Container)config.getControls()).add(option.getControls());
    }
  }

  public void build(Element e) {
    if (e != null) {
      NamedNodeMap n = e.getAttributes();
      for (int i = 0; i < n.getLength(); ++i) {
        Attr att = (Attr) n.item(i);
        setAttribute(att.getName(), att.getValue());
      }
      
//      NodeList l = e.getElementsByTagName("option");
//      for (int i=0,len=l.getLength();i<len;++i) {
//        Element option = (Element)l.item(i);
//        optionInitialValues.put(option.getAttribute("name"),Builder.getText(option));
//      }

        
      NodeList l = e.getChildNodes();
      for (int i=0,len=l.getLength();i<len;++i) {
        Node node = l.item(i);
        if (node.getNodeType() == Node.ELEMENT_NODE) {
          Element element = (Element) node;
          String name = element.getTagName();
          if (name.equals("option")) { //$NON-NLS-1$
            optionInitialValues.put(element.getAttribute("name"),Builder.getText(element)); //$NON-NLS-1$
          }
          else {
            Buildable b = null;
            try {
              b = Builder.create(element);
              b.addTo(this);
              add(b);
            }
            catch (Throwable err) {
              String msg = err.getMessage();
              if (msg == null) {
                msg = err.getClass().getName().substring(err.getClass().getName().lastIndexOf(".") + 1); //$NON-NLS-1$
              }
              System.err.println(b.toString());
              ErrorLog.log(err);
              JOptionPane.showMessageDialog
                (null,
                 Resources.getString("GlobalOptions.create_error",  //$NON-NLS-1$
                           ((Element) b).getTagName(),
                           GameModule.getGameModule().getDataArchive().getName(),
                           msg),
                 Resources.getString("GlobalOptions.error"), //$NON-NLS-1$
                 JOptionPane.ERROR_MESSAGE);
            }
          }
        }
      }
    }
  }

  public Element getBuildElement(Document doc) {
    Element e = super.getBuildElement(doc);
    for (Configurer c : optionConfigurers.values()) {
      Element option = doc.createElement("option"); //$NON-NLS-1$
      option.setAttribute("name", c.getKey()); //$NON-NLS-1$
      option.appendChild(doc.createTextNode(c.getValueString()));
      e.appendChild(option);
    }
    return e;
  }

  public Configurer getConfigurer() {
    if (config == null) {
      Configurer defaultConfig = super.getConfigurer();
      for (Configurer c : optionConfigurers.values()) {
        ((Container)defaultConfig.getControls()).add(c.getControls());
      }
    }
    return config;
  }

  public String getAttributeValueString(String key) {
    if (NON_OWNER_UNMASKABLE.equals(key)) {
      return nonOwnerUnmaskable;
    }
    else if (PROMPT_STRING.equals(key)) {
      return promptString;
    }
    else if (CENTER_ON_MOVE.equals(key)) {
      return centerOnMoves;
    }
    else if (AUTO_REPORT.equals(key)) {
      return autoReport;
    }
    else if (MARK_MOVED.equals(key)) {
      return markMoved;
    }
    else if (PLAYER_ID_FORMAT.equals(key)) {
      return playerIdFormat.getFormat();
    }
    else if (!optionConfigurers.containsKey(key)) {
      Object val = properties.get(key);
      return val != null ? val.toString() : null;
    }
    else {
      return null;
    }
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalOptions.htm"); //$NON-NLS-1$
  }

  public void removeFrom(Buildable parent) {
  }

  public void setAttribute(String key, Object value) {
    if (NON_OWNER_UNMASKABLE.equals(key)) {
      nonOwnerUnmaskable = (String) value;
      if (ALWAYS.equals(nonOwnerUnmaskable)) {
        ObscurableOptions.getInstance().allowAll();
      }
      else if (NEVER.equals(nonOwnerUnmaskable)) {
        ObscurableOptions.getInstance().allowNone();
      }
      else if (PROMPT.equals(nonOwnerUnmaskable)) {
        ObscurableOptions.getInstance().allowSome(promptString);
        GameModule.getGameModule().getGameState().addGameComponent(ObscurableOptions.getInstance());
        GameModule.getGameModule().addCommandEncoder(ObscurableOptions.getInstance());
      }
    }
    else if (PROMPT_STRING.equals(key)) {
      promptString = (String) value;
      ObscurableOptions.getInstance().setPrompt(promptString);
    }
    else if (CENTER_ON_MOVE.equals(key)) {
      centerOnMoves = (String) value;
      if (PROMPT.equals(centerOnMoves)) {
        BooleanConfigurer config = new BooleanConfigurer(CENTER_ON_MOVE, Resources.getString("GlobalOptions.center_on_move")); //$NON-NLS-1$
        GameModule.getGameModule().getPrefs().addOption(config);
      }
    }
    else if (AUTO_REPORT.equals(key)) {
      autoReport = (String) value;
      if (PROMPT.equals(autoReport)) {
        BooleanConfigurer config = new BooleanConfigurer(AUTO_REPORT, Resources.getString("GlobalOptions.auto_report")); //$NON-NLS-1$
        GameModule.getGameModule().getPrefs().addOption(config);
      }
    }
    else if (MARK_MOVED.equals(key)) {
      markMoved = (String) value;
      if (PROMPT.equals(markMoved)) {
        BooleanConfigurer config = new BooleanConfigurer(MARK_MOVED, Resources.getString("GlobalOptions.mark_moved")); //$NON-NLS-1$
        GameModule.getGameModule().getPrefs().addOption(config);
      }
    }
    else if (PLAYER_ID_FORMAT.equals(key)) {
      playerIdFormat.setFormat((String) value);
    }
    else if (optionConfigurers.containsKey(key)) {
      optionConfigurers.get(key).setValue(value);
    }
    else {
      properties.put(key, value);
    }
  }

  public boolean autoReportEnabled() {
    return isEnabled(autoReport, AUTO_REPORT);
  }

  public boolean centerOnOpponentsMove() {
    return isEnabled(centerOnMoves, CENTER_ON_MOVE);
  }

  public boolean isMarkMoveEnabled() {
    return isEnabled(markMoved, MARK_MOVED);
  }

  public String getPlayerId() {
    playerIdFormat.setProperty(PLAYER_NAME, (String) GameModule.getGameModule().getPrefs().getValue(GameModule.REAL_NAME));
    playerIdFormat.setProperty(PLAYER_SIDE, PlayerRoster.getMyLocalizedSide());
    return playerIdFormat.getText();
  }

  private boolean isEnabled(String attValue, String prefsPrompt) {
    if (ALWAYS.equals(attValue)) {
      return true;
    }
    else if (NEVER.equals(attValue)) {
      return false;
    }
    else {
      return Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(prefsPrompt));
    }
  }
}
