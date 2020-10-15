/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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

import VASSAL.tools.ProblemDialog;
import java.awt.Container;
import java.awt.dnd.DragSource;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.swing.JLabel;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

import org.apache.commons.lang3.SystemUtils;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.PieceMover;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.SingleChildInstance;
import VASSAL.configure.StringEnum;
import VASSAL.i18n.Resources;
import VASSAL.preferences.BasicPreference;
import VASSAL.preferences.BooleanPreference;
import VASSAL.preferences.DoublePreference;
import VASSAL.preferences.EnumPreference;
import VASSAL.preferences.IntegerPreference;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.StringPreference;
import VASSAL.preferences.TextPreference;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.FormattedString;
import VASSAL.tools.swing.SwingUtils;

public class GlobalOptions extends AbstractConfigurable {
  public static final String NON_OWNER_UNMASKABLE = "nonOwnerUnmaskable"; //$NON-NLS-1$
  public static final String PROMPT_STRING = "promptString"; //$NON-NLS-1$
  public static final String CENTER_ON_MOVE = "centerOnMove"; //$NON-NLS-1$
  public static final String CENTER_ON_MOVE_SENSITIVITY = "centerOnMoveSensitivity"; //$NON-NLS-1$
  public static final String MARK_MOVED = "markMoved"; //$NON-NLS-1$
  public static final String AUTO_REPORT = "autoReport"; //$NON-NLS-1$
  public static final String CHATTER_HTML_SUPPORT = "chatterHTMLSupport"; //$NON-NLS-1$
  public static final String ALWAYS = "Always"; //$NON-NLS-1$
  public static final String NEVER = "Never"; //$NON-NLS-1$
  public static final String PROMPT = "Use Preferences Setting"; //$NON-NLS-1$
  public static final String SINGLE_WINDOW = "singleWindow"; //$NON-NLS-1$
  public static final String MAXIMUM_HEAP = "maximumHeap"; //$NON-NLS-1$
  public static final String INITIAL_HEAP = "initialHeap"; //$NON-NLS-1$
  public static final String BUG_10295 = "bug10295"; //$NON-NLS-1$
  public static final String CLASSIC_MFD = "classicMfd"; //$NON-NLS-1$
  public static final String DRAG_THRESHOLD = "dragThreshold"; //$NON-NLS-1$
  public static final String MAC_LEGACY = "macLegacy"; //$NON-NLS-1$
  public static final String SOUND_GLOBAL_MUTE = "soundGlobalMute"; //NON-NLS
  public static final String SOUND_WAKEUP_MUTE = "soundWakeupMute"; //NON-NLS

  public static final String PLAYER_NAME = "PlayerName"; //$NON-NLS-1$
  public static final String PLAYER_NAME_ALT = "playerName"; //$NON-NLS-1$
  public static final String PLAYER_SIDE = "PlayerSide"; //$NON-NLS-1$
  public static final String PLAYER_SIDE_ALT = "playerSide"; //$NON-NLS-1$
  public static final String PLAYER_ID = "PlayerId"; //$NON-NLS-1$
  public static final String PLAYER_ID_ALT = "playerId"; //$NON-NLS-1$
  public static final String PLAYER_ID_FORMAT = "playerIdFormat"; //$NON-NLS-1$

  public static final boolean FORCE_MAC_LEGACY = true; //BR// Keeps Mac key translation "waiting in the wings"

  private String promptString = Resources.getString("GlobalOptions.opponents_can_unmask_my_pieces");
  private String nonOwnerUnmaskable = NEVER;
  private final String centerOnMoves = PROMPT;
  private String autoReport = ALWAYS;
  private String markMoved = NEVER;
  private String chatterHTMLSupport = NEVER;
  
  private int dragThreshold = 10;
  
  private boolean macLegacy;
  private boolean soundGlobalMute = false;
  private boolean soundWakeupMute = false;

  private final Map<String, Object> properties = new HashMap<>();
  private static final Map<String, Configurer> OPTION_CONFIGURERS = new LinkedHashMap<>();
  private static final Properties OPTION_INITIAL_VALUES = new Properties();

  private final FormattedString playerIdFormat = new FormattedString("$" + PLAYER_NAME + "$"); //$NON-NLS-1$ //$NON-NLS-2$

  private static GlobalOptions instance = new GlobalOptions();
  private boolean useSingleWindow;
  
  private boolean useClassicMoveFixedDistance = false;
  private BooleanConfigurer classicMfd;  

  private static void setInstance(GlobalOptions go) {
    instance = go;
  }

  @Override
  public void addTo(Buildable parent) {
    setInstance(this);

    final GameModule gm = GameModule.getGameModule();
    final Prefs prefs = gm.getPrefs();

    // should this moudule use a combined main window?
    final BooleanConfigurer combConf = new BooleanConfigurer(
      SINGLE_WINDOW,
      Resources.getString("GlobalOptions.use_combined"),  //$NON-NLS-1$
      Boolean.TRUE
    );
    prefs.addOption(combConf);
    useSingleWindow = !Boolean.FALSE.equals(combConf.getValue());

    // the initial heap size for this module
    final IntConfigurer initHeapConf = new IntConfigurer(
      INITIAL_HEAP,
      Resources.getString("GlobalOptions.initial_heap"),  //$NON-NLS-1$
      256
    );
    prefs.addOption(initHeapConf);

    // the maximum heap size for this module
    final IntConfigurer maxHeapConf = new IntConfigurer(
      MAXIMUM_HEAP,
      Resources.getString("GlobalOptions.maximum_heap"),  //$NON-NLS-1$
      512
    );
    prefs.addOption(maxHeapConf);

    // Bug 10295: Sometimes, for unknown reasons, the native drag handler
    // fails to draw images properly on Windows. This lets the user select
    // the drag handler to use.
    if (SystemUtils.IS_OS_WINDOWS) {
      final BooleanConfigurer bug10295Conf = new BooleanConfigurer(
        BUG_10295,
        Resources.getString("GlobalOptions.bug10295"),
        Boolean.FALSE
      );

      final boolean dragHandlerNoImageNecessary =
        Boolean.TRUE.equals(bug10295Conf.getValue()) &&
        !(PieceMover.AbstractDragHandler.getTheDragHandler()
          instanceof PieceMover.DragHandlerNoImage);

      if (dragHandlerNoImageNecessary) {
        PieceMover.AbstractDragHandler.setTheDragHandler(new PieceMover.DragHandlerNoImage());
      }

      bug10295Conf.addPropertyChangeListener(e -> PieceMover.AbstractDragHandler.setTheDragHandler(
        (Boolean.TRUE.equals(e.getNewValue()) ||
         !DragSource.isDragImageSupported()) ?
         new PieceMover.DragHandlerNoImage() :
         new PieceMover.DragHandler()
      ));

      prefs.addOption(Resources.getString("Prefs.compatibility_tab"), bug10295Conf);
    }
    
    // Move Fixed Distance trait (Translate) has been substantially re-written.
    // Use new version by default. User may over-ride to use old buggy behaviour.
    classicMfd = new BooleanConfigurer(
        CLASSIC_MFD,
        Resources.getString("GlobalOptions.classic_mfd"),
        Boolean.FALSE
      );
    classicMfd.addPropertyChangeListener(evt -> setUseClassicMoveFixedDistance(classicMfd.getValueBoolean()));
    prefs.addOption(Resources.getString("Prefs.compatibility_tab"), classicMfd);

    //BR// Drag Threshold
    final IntConfigurer dragThresholdConf = new IntConfigurer(
      DRAG_THRESHOLD,
      Resources.getString("GlobalOptions.mouse_drag_threshold"),  //$NON-NLS-1$
      10
    );
    dragThresholdConf.addPropertyChangeListener(e -> {
      dragThreshold = dragThresholdConf.getIntValue(10);
      System.setProperty("awt.dnd.drag.threshold", Integer.toString(dragThreshold));
    });
    prefs.addOption(dragThresholdConf);
    
    //BR// Mac Legacy (essentially swaps Control and Command functions to their "old, bad, pre-3.3.3" mappings)
    final BooleanConfigurer macLegacyConf = new BooleanConfigurer(
        MAC_LEGACY,
        Resources.getString("GlobalOptions.mac_legacy"),
        Boolean.FALSE);
    macLegacyConf.addPropertyChangeListener(evt -> setPrefMacLegacy(macLegacyConf.getValueBoolean()));

    if (!FORCE_MAC_LEGACY && SystemUtils.IS_OS_MAC_OSX) {
      // Only need to *display* this preference if we're running on a Mac.
      prefs.addOption(Resources.getString("Prefs.compatibility_tab"), macLegacyConf);
    }
    
    BooleanConfigurer config = new BooleanConfigurer(CENTER_ON_MOVE, Resources.getString("GlobalOptions.center_on_move"), Boolean.TRUE); //$NON-NLS-1$
    prefs.addOption(config);

    //CC// center_on_move_border_proximity_pct (is the pct of distance from border to center of window that triggers a recenter
    final IntConfigurer pctRecenterOn = new IntConfigurer(CENTER_ON_MOVE_SENSITIVITY,
      Resources.getString("GlobalOptions.center_on_move_sensitivity"), 10); //$NON-NLS-1$
    prefs.addOption(pctRecenterOn);

    validator = new SingleChildInstance(gm, getClass());

    final BooleanConfigurer soundWakeupMuteConf = new BooleanConfigurer(
      SOUND_WAKEUP_MUTE,
      Resources.getString("GlobalOptions.sound_wakeup_mute"),
      Boolean.FALSE);
    soundWakeupMuteConf.addPropertyChangeListener(evt -> setSoundWakeupMute(soundWakeupMuteConf.getValueBoolean()));
    prefs.addOption(Resources.getString("Prefs.sounds_tab"), soundWakeupMuteConf);

    final BooleanConfigurer soundGlobalMuteConf = new BooleanConfigurer(
      SOUND_GLOBAL_MUTE,
      Resources.getString("GlobalOptions.sound_global_mute"),
      Boolean.FALSE);
    soundGlobalMuteConf.addPropertyChangeListener(evt -> setSoundGlobalMute(soundGlobalMuteConf.getValueBoolean()));
    prefs.addOption(Resources.getString("Prefs.sounds_tab"), soundGlobalMuteConf);
  }

  public static GlobalOptions getInstance() {
    return instance;
  }

  public boolean isUseSingleWindow() {
    return useSingleWindow;
  }

  public boolean isUseClassicMoveFixedDistance() {
    return useClassicMoveFixedDistance;
  }

  public void setUseClassicMoveFixedDistance(boolean b) {
    useClassicMoveFixedDistance = b;
  }

  /** @deprecated No replacement */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public boolean isAveragedScaling() {
    ProblemDialog.showDeprecated("2020-08-06");  //NON-NLS
    return true;
  }

  public void setPrefMacLegacy(boolean b) {
    macLegacy = b;
    // Tell SwingUtils we've changed our mind about Macs
    SwingUtils.setMacLegacy(b);
    // Since we've changed our key mapping paradigm, we need to refresh all the keystroke listeners.
    GameModule.getGameModule().refreshKeyStrokeListeners(); 
  }

  public boolean getPrefMacLegacy() {
    return macLegacy;
  }

  public void setSoundGlobalMute(Boolean b) {
    soundGlobalMute = b;
  }

  public Boolean isSoundGlobalMute() {
    return soundGlobalMute;
  }

  public void setSoundWakeupMute(Boolean b) {
    soundWakeupMute = b;
  }

  public Boolean isSoundWakeupMute() {
    return soundWakeupMute;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GlobalOption.component_type"); //$NON-NLS-1$
  }

  public static class Prompt extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, NEVER, PROMPT};
    }
  }

  public static class PromptOnOff extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, NEVER};
    }
  }

  public static class PlayerIdFormatConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{PLAYER_NAME, PLAYER_SIDE});
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{
      StringPreference.class,
      TextPreference.class,
      EnumPreference.class,
      IntegerPreference.class,
      DoublePreference.class,
      BooleanPreference.class
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.GlobalOption.nonowner_unmask"), //$NON-NLS-1$
      null,
      Resources.getString("Editor.GlobalOption.autoreport_moves"), //$NON-NLS-1$
      Resources.getString("Editor.GlobalOption.playerid_format"), //$NON-NLS-1$
      Resources.getString("Editor.GlobalOption.chatter_html_support") //$NON-NLS-1$
    };
  }

  @Override
  public String[] getAttributeNames() {
    final ArrayList<String> attributes = new ArrayList<>(
      Arrays.asList(
        NON_OWNER_UNMASKABLE,
        PROMPT_STRING,
        AUTO_REPORT,
        PLAYER_ID_FORMAT,
        CHATTER_HTML_SUPPORT
      )
    );

    attributes.addAll(properties.keySet());

    return attributes.toArray(new String[0]);
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      Prompt.class,
      null,
      Prompt.class,
      PlayerIdFormatConfig.class,
      PromptOnOff.class
    };
  }

  /**
   * Components may use GlobalOptions to store generic global attributes.
   * This method registers the given key as an attribute of the GlobalOptions
   * with the given type.
   */
  public void addOption(Configurer option) {
    OPTION_CONFIGURERS.put(option.getKey(), option);
    Object initValue = OPTION_INITIAL_VALUES.get(option.getKey());

    if (initValue instanceof String) {
      option.setValue((String)initValue);
    }

    if (config != null) {
      ((Container)config.getControls()).add(option.getControls());
    }
  }

  @Override
  public void build(Element e) {
    if (e == null) return;

    final NamedNodeMap nnm = e.getAttributes();
    for (int i = 0; i < nnm.getLength(); ++i) {
      final Attr att = (Attr) nnm.item(i);
      setAttribute(att.getName(), att.getValue());
    }

    for (Node n = e.getFirstChild(); n != null; n = n.getNextSibling()) {
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        final Element element = (Element) n;
        if ("option".equals(element.getTagName())) { //$NON-NLS-1$
          final String optionName = element.getAttribute("name"); //$NON-NLS-1$
          final String value = Builder.getText(element);
          OPTION_INITIAL_VALUES.put(optionName, value);
          // Update the Configurer value if it is already registered
          final Configurer config = OPTION_CONFIGURERS.get(optionName);
          if (config != null) {
            config.setValue(value);
          }
        }
        else {
          try {
            final Buildable b = Builder.create(element);
            b.addTo(this);
            add(b);
          }
          catch (IllegalBuildException ex) {
            ErrorDialog.bug(ex);
          }
        }
      }
    }
  }

  @Override
  public Element getBuildElement(Document doc) {
    final Element e = super.getBuildElement(doc);
    for (Configurer c : OPTION_CONFIGURERS.values()) {
      final Element option = doc.createElement("option"); //$NON-NLS-1$
      option.setAttribute("name", c.getKey()); //$NON-NLS-1$
      option.appendChild(doc.createTextNode(c.getValueString()));
      e.appendChild(option);
    }
    return e;
  }

  @Override
  public Configurer getConfigurer() {
    if (config == null) {
      final Configurer defaultConfig = super.getConfigurer();
      for (Configurer c : OPTION_CONFIGURERS.values()) {
        final Container container = (Container) defaultConfig.getControls();
        final String name = c.getName();
        container.add(new JLabel(name));
        c.setName("");
        container.add(c.getControls(), "wrap"); // NON-NLS
        c.setName(name);
      }
    }
    return config;
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NON_OWNER_UNMASKABLE.equals(key)) {
      return nonOwnerUnmaskable;
    }
    else if (PROMPT_STRING.equals(key)) {
      return promptString;
    }
    else if (CHATTER_HTML_SUPPORT.equals(key)) {
      if (PROMPT.equals(chatterHTMLSupport)) {
        return NEVER; //BR// So peeps with the old/bad "preference" option will get a safe result.
      }
      return chatterHTMLSupport;
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
    else if (DRAG_THRESHOLD.equals(key)) {
      return Integer.toString(dragThreshold);  
    }
    else if (!OPTION_CONFIGURERS.containsKey(key)) {
      Object val = properties.get(key);
      return val != null ? val.toString() : null;
    }
    else {
      return null;
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalOptions.html"); //$NON-NLS-1$
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
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
    else if (CHATTER_HTML_SUPPORT.equals(key)) {
      chatterHTMLSupport = (String) value;
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
    else if (OPTION_CONFIGURERS.containsKey(key)) {
      OPTION_CONFIGURERS.get(key).setValue(value);
    }
    else {
      properties.put(key, value);
    }
  }

  public boolean autoReportEnabled() {
    return isEnabled(autoReport, AUTO_REPORT);
  }

  public boolean centerOnOpponentsMove() {
    return Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(CENTER_ON_MOVE));
  }

  public double centerOnOpponentsMoveSensitivity() {
    int sensitivity = (Integer) GameModule.getGameModule().getPrefs().getValue(CENTER_ON_MOVE_SENSITIVITY);
    if (sensitivity > 100) {
      sensitivity = 100;
    }
    else if (sensitivity < 0) {
      sensitivity = 0;
    }
    return sensitivity;
  }

  public String chatterHTMLSetting() {
    return chatterHTMLSupport;
  }
  
  public boolean chatterHTMLSupport() {
    return isEnabled(chatterHTMLSupport, CHATTER_HTML_SUPPORT);
  }

  public boolean isMarkMoveEnabled() {
    return isEnabled(markMoved, MARK_MOVED);
  }
  
  public int getDragThreshold() {
    return dragThreshold;
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
    else if (NEVER.equals(attValue) || CHATTER_HTML_SUPPORT.equals(prefsPrompt)) {
      return false;
    }
    else {
      return Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(prefsPrompt));
    }
  }

  /**
   * Implement PropertyNameSource - Expose our preference names
   */
  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    for (Buildable b : getBuildables()) {
      if (b instanceof BasicPreference) {
        l.add(((BasicPreference) b).getVariableName());
      }
    }
    return l;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(playerIdFormat.getFormat());
  }
}
