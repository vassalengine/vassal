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

import VASSAL.configure.IconConfigurer;
import VASSAL.tools.ProblemDialog;
import java.awt.Container;
import java.awt.dnd.DragSource;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
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

/**
 * GlobalOptions is a junction point for several flavors of "preferences"-related categories. It also configures the
 * toolbar buttons basic VASSAL functions: Undo, Step Forward, and show/hide Servers. Global Options is a Singleton.
 * <br>(1) Its <i>attributes</i>, set in the Editor, control the module-designer-set "hybrid preference" categories
 * (Allowing non-owners to unmask pieces, Enabling HTML chat, format of player ID in reports), determining whether the
 * preference in question is forced on or off, or whether a player preference is used.
 * <br>(2) Its <i>sub-components</i> allow the module designer to "create custom preferences" which appear in their
 * own tab.
 * <br>(3) Many of its <i>fields</i>, configured mostly in the {@link #addTo} method, supply the majority of the
 * preferences shown on the "General" tab, though some of that tab's preferences are also set elsewhere -- see:
 * <br>{@link Prefs} - reading/writing the preference file, maintaining lists of configurers for tabs
 * <br>{@link Prefs#initSharedGlobalPrefs} - disable d3d pipeline, wizard support
 * <br>{@link BasicLogger} - configurers for Undo & Step Forward. Adds logging-related preferences to pane
 * <br>{@link VASSAL.tools.AdjustableSpeedScrollPane} - scroll increment
 */
public class GlobalOptions extends AbstractConfigurable {
  // Hybrid preferences (designer-set attribute OR possibly a player preference)
  public static final String NON_OWNER_UNMASKABLE = "nonOwnerUnmaskable"; //$NON-NLS-1$
  public static final String PROMPT_STRING = "promptString"; //$NON-NLS-1$
  public static final String MARK_MOVED = "markMoved"; //$NON-NLS-1$
  public static final String AUTO_REPORT = "autoReport"; //$NON-NLS-1$
  public static final String CHATTER_HTML_SUPPORT = "chatterHTMLSupport"; //$NON-NLS-1$
  public static final String HOTKEYS_ON_CLOSED_WINDOWS = "hotKeysOnClosedWindows"; //NON-NLS
  public static final String TRANSLATABLE_SUPPORT = "translatableSupport"; //NON-NLS

  // Hybrid preference settings
  public static final String ALWAYS = "Always"; //$NON-NLS-1$
  public static final String NEVER = "Never"; //$NON-NLS-1$
  public static final String PROMPT = "Use Preferences Setting"; //$NON-NLS-1$

  // General Tab preferences
  public static final String CENTER_ON_MOVE = "centerOnMove"; //$NON-NLS-1$
  public static final String CENTER_ON_MOVE_SENSITIVITY = "centerOnMoveSensitivity"; //$NON-NLS-1$
  public static final String SINGLE_WINDOW = "singleWindow"; //$NON-NLS-1$
  public static final String MAXIMUM_HEAP = "maximumHeap"; //$NON-NLS-1$
  public static final String DRAG_THRESHOLD = "dragThreshold"; //$NON-NLS-1$
  public static final String STACK_VIEWER_ORDER = "stackViewerOrder"; //NON-NLS

  // Compatibility Tab preferences
  public static final String BUG_10295 = "bug10295"; //$NON-NLS-1$
  public static final String CLASSIC_MFD = "classicMfd"; //$NON-NLS-1$
  public static final String MAC_LEGACY = "macLegacy"; //$NON-NLS-1$
  public static final String OLD_CONTINUATION = "oldContinuation"; //NON-NLS
  public static final String SEND_TO_LOCATION_MOVEMENT_TRAILS = "stlMovementTrails"; //NON-NLS

  // Sound Tab preferences
  public static final String SOUND_GLOBAL_MUTE = "soundGlobalMute"; //NON-NLS
  public static final String SOUND_WAKEUP_MUTE = "soundWakeupMute"; //NON-NLS

  // Player ID format configuration
  public static final String PLAYER_NAME = "PlayerName"; //$NON-NLS-1$
  public static final String PLAYER_NAME_ALT = "playerName"; //$NON-NLS-1$
  public static final String PLAYER_SIDE = "PlayerSide"; //$NON-NLS-1$
  public static final String PLAYER_SIDE_ALT = "playerSide"; //$NON-NLS-1$
  public static final String PLAYER_ID = "PlayerId"; //$NON-NLS-1$
  public static final String PLAYER_ID_ALT = "playerId"; //$NON-NLS-1$
  public static final String PLAYER_ID_FORMAT = "playerIdFormat"; //$NON-NLS-1$

  public static final boolean FORCE_MAC_LEGACY = true; //BR// Keeps Mac key translation "waiting in the wings"

  @Deprecated(since = "2020-10-21", forRemoval = true)
  public static final String INITIAL_HEAP = "initialHeap"; //$NON-NLS-1$

  /************************************************************
   * Attributes configured by module designer
   ************************************************************/
  private String nonOwnerUnmaskable = NEVER; // "Pieces can be unmasked by non-owners" -> defaults to forced off
  private String autoReport = ALWAYS;        // "Auto-report moves" -> defaults to forced on
  private String markMoved = NEVER;          // **NO LONGER USED**
  private String chatterHTMLSupport = NEVER; // "Enable HTML Chat" - > defaults to forced off
  private String hotKeysOnClosedWindows = NEVER; // Hotkeys on Closed Windows -> defaults to off

  // Configurable prompt string for unmask-my-pieces
  private String promptString = Resources.getString("GlobalOptions.opponents_can_unmask_my_pieces");

  // Configurable Player ID format in messages
  private final FormattedString playerIdFormat = new FormattedString("$" + PLAYER_NAME + "$");

  /************************************************************
   * Standard player preferences
   ************************************************************/
  private int dragThreshold = 10;            // Drag threshold for distinguishing drags from clicks

  private boolean macLegacy;                 // Mac Legacy support enabled? (presently forced on - will be deprecated or restored to service)
  private boolean soundGlobalMute = false;   // Global "mute all sounds" flag
  private boolean soundWakeupMute = false;   // Separate Mute for the "wakeup" sound
  private boolean useSingleWindow;           // If true, first map should dock to the main module window (along with the Chatter)
  private boolean useClassicMoveFixedDistance = false; // Compatibility preference for move-fixed distance
  private boolean warnOldContinuation = true; // Warn when using old-style Load Continuation (compatibility)

  /************************************************************
   * Custom preferences
   ************************************************************/
  private final Map<String, Object> properties = new HashMap<>();
  private static final Map<String, Configurer> OPTION_CONFIGURERS = new LinkedHashMap<>();
  private static final Properties OPTION_INITIAL_VALUES = new Properties();

  // Manage our singleton instance
  private static GlobalOptions instance = new GlobalOptions();

  /** @param go sets our singleton instance */
  private static void setInstance(GlobalOptions go) {
    instance = go;
  }

  /** @return our singleton instance */
  public static GlobalOptions getInstance() {
    return instance;
  }

  /**
   * GlobalOptions are added to the {@link GameModule}. This method creates
   * the configurers for the "General" tab of the preference dialog, and some
   * for the "Compatibility" tab.
   * @param parent our GameModule object
   */
  @Override
  public void addTo(Buildable parent) {
    setInstance(this);

    final GameModule gm = GameModule.getGameModule();
    final Prefs prefs = gm.getPrefs();
    validator = new SingleChildInstance(gm, getClass());

    /////////////////////
    // GENERAL TAB TAB //
    /////////////////////

    // should this module use a combined main window? (first map window docks to main module window w/ Chatter)
    final BooleanConfigurer combConf = new BooleanConfigurer(
      SINGLE_WINDOW,
      Resources.getString("GlobalOptions.use_combined"),  //$NON-NLS-1$
      Boolean.TRUE
    );
    prefs.addOption(combConf);
    useSingleWindow = !Boolean.FALSE.equals(combConf.getValue());

    // the maximum heap size for this module
    final IntConfigurer maxHeapConf = new IntConfigurer(
      MAXIMUM_HEAP,
      Resources.getString("GlobalOptions.maximum_heap"),  //$NON-NLS-1$
      512
    );
    prefs.addOption(maxHeapConf);

    //BR// Drag Threshold - # pixels mouse must move to distinguish drag from click
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


    // Preference to center on opponent's moves (used to be module-designer-set attribute, now always a player preference)
    final BooleanConfigurer config = new BooleanConfigurer(CENTER_ON_MOVE, Resources.getString("GlobalOptions.center_on_move"), Boolean.TRUE); //$NON-NLS-1$
    prefs.addOption(config);

    // Preference to reverse the left-to-right order for the Mouseover Stack Viewer (CounterDetailViewer)
    final BooleanConfigurer stackViewerOrder = new BooleanConfigurer(STACK_VIEWER_ORDER, Resources.getString("GlobalOptions.stack_viewer_order"), Boolean.FALSE);
    prefs.addOption(stackViewerOrder);

    //CC// Center-on-Moves Sensitivity (is the pct of distance from border to center of window that triggers a recenter)
    final IntConfigurer pctRecenterOn = new IntConfigurer(CENTER_ON_MOVE_SENSITIVITY,
      Resources.getString("GlobalOptions.center_on_move_sensitivity"), 10); //$NON-NLS-1$
    prefs.addOption(pctRecenterOn);

    ///////////////////////
    // COMPATIBILITY TAB //
    ///////////////////////

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
    final BooleanConfigurer classicMfd = new BooleanConfigurer(
      CLASSIC_MFD,
      Resources.getString("GlobalOptions.classic_mfd"),
      Boolean.FALSE
    );
    classicMfd.addPropertyChangeListener(evt -> setUseClassicMoveFixedDistance(classicMfd.getValueBoolean()));
    prefs.addOption(Resources.getString("Prefs.compatibility_tab"), classicMfd);

    //BR// Mac Legacy (essentially swaps Control and Command functions to their "old, bad, pre-3.3.3" mappings)
    final BooleanConfigurer macLegacyConf = new BooleanConfigurer(
      MAC_LEGACY,
      Resources.getString("GlobalOptions.mac_legacy"),
      Boolean.FALSE);
    macLegacyConf.addPropertyChangeListener(evt -> setPrefMacLegacy(macLegacyConf.getValueBoolean()));

    if (!FORCE_MAC_LEGACY && SystemUtils.IS_OS_MAC) {
      // Only need to *display* this preference if we're running on a Mac.
      prefs.addOption(Resources.getString("Prefs.compatibility_tab"), macLegacyConf);
    }

    final BooleanConfigurer oldContinuationConf = new BooleanConfigurer(
      OLD_CONTINUATION,
      Resources.getString("GlobalOptions.old_continuation"),
      Boolean.TRUE
    );
    oldContinuationConf.addPropertyChangeListener(evt -> setWarnOldContinuation(oldContinuationConf.getValueBoolean()));
    prefs.addOption(Resources.getString("Prefs.compatibility_tab"), oldContinuationConf);

    // Send to Location to generate movement trails. Note the default is TRUE which is a change from existing (broken) behaviour
    // Affected module owners will need to turn this preference off.
    final BooleanConfigurer sendToLocationMovementTrails = new BooleanConfigurer(
      SEND_TO_LOCATION_MOVEMENT_TRAILS,
      Resources.getString("GlobalOptions.send_to_location_movement_trails"),
      Boolean.TRUE
    );
    prefs.addOption(Resources.getString("Prefs.compatibility_tab"), sendToLocationMovementTrails);

    ////////////////
    // SOUNDS TAB //
    ////////////////
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

  /** @return true if preference to dock first map window to main module window with Chatter is selected */
  public boolean isUseSingleWindow() {
    return useSingleWindow;
  }

  /** @return true if compatibility version of Move Fixed Distance is selected */
  public boolean isUseClassicMoveFixedDistance() {
    return useClassicMoveFixedDistance;
  }

  /** @param b sets the compatibility Move Fixed Distance preference */
  public void setUseClassicMoveFixedDistance(boolean b) {
    useClassicMoveFixedDistance = b;
  }

  /** @deprecated No replacement */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public boolean isAveragedScaling() {
    ProblemDialog.showDeprecated("2020-08-06");  //NON-NLS
    return true;
  }

  /** @param b sets the Mac Legacy compatibility preference */
  public void setPrefMacLegacy(boolean b) {
    macLegacy = b;
    // Tell SwingUtils we've changed our mind about Macs
    SwingUtils.setMacLegacy(b);
    // Since we've changed our key mapping paradigm, we need to refresh all the keystroke listeners.
    GameModule.getGameModule().refreshKeyStrokeListeners();
  }

  /** @return the Mac Legacy compatibility preference */
  public boolean getPrefMacLegacy() {
    return macLegacy;
  }


  /** @param b sets the warnOldContinuation compatibility preference */
  public void setWarnOldContinuation(boolean b) {
    warnOldContinuation = b;
  }

  /** @return the warnOldContinuation compatibility preference */
  public boolean isWarnOldContinuation() {
    return warnOldContinuation;
  }

  /** @param b sets the global sound mute preference */
  public void setSoundGlobalMute(Boolean b) {
    soundGlobalMute = b;
  }

  /** @return true if sounds should be muted globally  */
  public Boolean isSoundGlobalMute() {
    return soundGlobalMute;
  }

  /** @param b sets the mute for the wake-up sound */
  public void setSoundWakeupMute(Boolean b) {
    soundWakeupMute = b;
  }

  /** @return true if the wake-up sound should be muted */
  public Boolean isSoundWakeupMute() {
    return soundWakeupMute;
  }

  /** @return component name to be displayed in the editor, e.g. [Global Options] */
  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GlobalOption.component_type"); //$NON-NLS-1$
  }

  /**
   * The Prompt class allows certain attribute settings to be configured by the module designer:
   * ALWAYS = the "preference" is forced on
   * NEVER  = the "preference" is forced off
   * PROMPT = the player actually receives a preference on the General tab to control this behavior
   */
  public static class Prompt extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, NEVER, PROMPT};
    }
  }

  /**
   * The PromptOnOff class allows certain attribute settings to be configured by the module designer:
   * ALWAYS = the "preference" is forced on
   * NEVER  = the "preference" is forced off
   */
  public static class PromptOnOff extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, NEVER};
    }
  }

  /**
   * Configurer for the Player's ID
   */
  public static class PlayerIdFormatConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{PLAYER_NAME, PLAYER_SIDE});
    }
  }

  /**
   * @return classes allowed to be added as subcomponents (in this case, custom preferences)
   */
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

  /**
   * @return Display strings describing attributes available for configuration
   */
  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.GlobalOption.nonowner_unmask"), //$NON-NLS-1$
      null,
      Resources.getString("Editor.GlobalOption.autoreport_moves"), //$NON-NLS-1$
      Resources.getString("Editor.GlobalOption.playerid_format"), //$NON-NLS-1$
      Resources.getString("Editor.GlobalOption.chatter_html_support"), //$NON-NLS-1$
      Resources.getString("Editor.GlobalOption.hot_keys_on_closed_windows") //NON-NLS
    };
  }

  /**
   * @return Attribute keys for XML buildFile
   */
  @Override
  public String[] getAttributeNames() {
    final ArrayList<String> attributes = new ArrayList<>(
      Arrays.asList(
        NON_OWNER_UNMASKABLE,
        PROMPT_STRING,
        AUTO_REPORT,
        PLAYER_ID_FORMAT,
        CHATTER_HTML_SUPPORT,
        HOTKEYS_ON_CLOSED_WINDOWS
      )
    );

    attributes.addAll(properties.keySet());

    return attributes.toArray(new String[0]);
  }

  /**
   * @return configurer classes for our attributes
   */
  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      Prompt.class,
      null,
      Prompt.class,
      PlayerIdFormatConfig.class,
      PromptOnOff.class,
      PromptOnOff.class
    };
  }

  /**
   * Components may use GlobalOptions to store generic global attributes.
   * This method registers the given key as an attribute of the GlobalOptions
   * with the given type.
   * @param option - configurer to add
   */
  public void addOption(Configurer option) {
    OPTION_CONFIGURERS.put(option.getKey(), option);
    final Object initValue = OPTION_INITIAL_VALUES.get(option.getKey());

    if (initValue instanceof String) {
      option.setValue((String)initValue);
    }

    if (config != null) {
      ((Container)config.getControls()).add(option.getControls());
    }
  }

  /**
   * Builds our component from the buildFile. In our case it mainly involves checkout for subcomponents which
   * represent "custom preferences" and appropriately creating configurers and a preferences tab for them.
   * @param e our XML element from the buildFile
   */
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

  /**
   * Packages any "custom preferences" back up into XML for the buildFile
   * @param doc the XML document we are writing to
   * @return XML Element
   */
  @Override
  public Element getBuildElement(Document doc) {
    final Element e = super.getBuildElement(doc);
    for (final Configurer c : OPTION_CONFIGURERS.values()) {
      final Element option = doc.createElement("option"); //$NON-NLS-1$
      option.setAttribute("name", c.getKey()); //$NON-NLS-1$
      option.appendChild(doc.createTextNode(c.getValueString()));
      e.appendChild(option);
    }
    return e;
  }

  /**
   * @return a configurer including all custom preferences
   */
  @Override
  public Configurer getConfigurer() {
    if (config == null) {
      final Configurer defaultConfig = super.getConfigurer();
      for (final Configurer c : OPTION_CONFIGURERS.values()) {
        final Container container = (Container) defaultConfig.getControls();
        final String name = c.getName();
        final JLabel label = new JLabel(name);
        c.setName("");
        label.setLabelFor(c.getControls());
        container.add(label);
        container.add(c.getControls(), "wrap"); // NON-NLS
        c.setName(name);
      }
    }
    return config;
  }

  /**
   * Returns current setting of an attribute in String form
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   * @return String value of attribute
   */
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
    else if (HOTKEYS_ON_CLOSED_WINDOWS.equals(key)) {
      return hotKeysOnClosedWindows;
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
      final Object val = properties.get(key);
      return val != null ? val.toString() : null;
    }
    else {
      return null;
    }
  }

  /**
   * @return Help file to be used on this component's configuration dialog
   */
  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalOptions.html"); //$NON-NLS-1$
  }

  /**
   * Removes us from our parent component
   * @param parent our parent (GameModule)
   */
  @Override
  public void removeFrom(Buildable parent) {
  }

  /**
   * Sets a new value for one of our attributes. Some special attention is needed when one of the designer-controlled
   * attributes changes, as it may necessitate the showing/hiding of a new user preference checkbox configurer
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   * @param value If the <code>value</code> parameter is a String, it will be the value returned by {@link #getAttributeValueString} for the same
   *              <code>key</code>. If the implementing class extends {@link AbstractConfigurable}, then <code>value</code> can also be an instance of
   */
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
    }
    else if (CHATTER_HTML_SUPPORT.equals(key)) {
      chatterHTMLSupport = (String) value;
    }
    else if (HOTKEYS_ON_CLOSED_WINDOWS.equals(key)) {
      hotKeysOnClosedWindows = (String) value;
    }
    else if (AUTO_REPORT.equals(key)) {
      autoReport = (String) value;
      if (PROMPT.equals(autoReport)) {
        final BooleanConfigurer config = new BooleanConfigurer(AUTO_REPORT, Resources.getString("GlobalOptions.auto_report")); //$NON-NLS-1$
        GameModule.getGameModule().getPrefs().addOption(config);
      }
    }
    else if (MARK_MOVED.equals(key)) {
      markMoved = (String) value;
      if (PROMPT.equals(markMoved)) {
        final BooleanConfigurer config = new BooleanConfigurer(MARK_MOVED, Resources.getString("GlobalOptions.mark_moved")); //$NON-NLS-1$
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

  /** @return true if auto-reporting moves is enabled (designer-setting or user-pref, depending) */
  public boolean autoReportEnabled() {
    return isEnabled(autoReport, AUTO_REPORT);
  }

  /** @return true if center-on-opponents-move user pref is selected (no longer a designer setting) */
  public boolean centerOnOpponentsMove() {
    return Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(CENTER_ON_MOVE));
  }

  /** @return true if stack viewer should reverse left-to-right order */
  public boolean isReverseStackViewerOrder() {
    return Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(STACK_VIEWER_ORDER));
  }

  /** @return percent-distance-from-center sensitivity for centering on opponent's move */
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

  /** @return designer's setting for enabling HTML chat */
  public String chatterHTMLSetting() {
    return chatterHTMLSupport;
  }

  /** @return true if chat is enabled in HTML (uses designer setting only) */
  public boolean chatterHTMLSupport() {
    return isEnabled(chatterHTMLSupport, CHATTER_HTML_SUPPORT);
  }

  /** @return designer's setting for allowing hotkeys on closed windows */
  public boolean isHotKeysOnClosedWindows() {
    return isEnabled(hotKeysOnClosedWindows, HOTKEYS_ON_CLOSED_WINDOWS);
  }

  /** @return - NO LONGER USED */
  public boolean isMarkMoveEnabled() {
    return isEnabled(markMoved, MARK_MOVED);
  }

  /** @return drag threshold - pixels of movement required to distinguish drag from click */
  public int getDragThreshold() {
    return dragThreshold;
  }

  /** @return player ID for current player using the designer-configured format */
  public String getPlayerId() {
    playerIdFormat.setProperty(PLAYER_NAME, (String) GameModule.getGameModule().getPrefs().getValue(GameModule.REAL_NAME));
    playerIdFormat.setProperty(PLAYER_SIDE, PlayerRoster.getMyLocalizedSide());
    return playerIdFormat.getText();
  }

  /** @return whether specific hybrid preference is enabled (could be designer-forced setting, could be player preference) */
  private boolean isEnabled(String attValue, String prefsPrompt) {
    if (ALWAYS.equals(attValue)) {
      return true;
    }
    else if (NEVER.equals(attValue) || CHATTER_HTML_SUPPORT.equals(prefsPrompt) || HOTKEYS_ON_CLOSED_WINDOWS.equals(prefsPrompt)) {
      return false;
    }
    else {
      return Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(prefsPrompt));
    }
  }

  /**
   * Implement PropertyNameSource - Expose our preference names
   * @return property names for custom preferences
   */
  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    for (final Buildable b : getBuildables()) {
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

  /**
   * Our Option Configurers (undo button, server button, step-forward button) potentially have icons to add the references for
   * @param s Collection to add image names to
   */
  @Override
  public void addImageNamesRecursively(Collection<String> s) {
    for (final Configurer c : OPTION_CONFIGURERS.values()) {
      if (!(c instanceof IconConfigurer)) {
        continue;
      }
      s.add(c.getValueString());
    }
  }
}
