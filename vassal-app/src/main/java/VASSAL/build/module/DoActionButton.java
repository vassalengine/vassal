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

import VASSAL.configure.NamedKeyStrokeArrayConfigurer;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.command.PlayAudioClipCommand;
import VASSAL.configure.AudioClipConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.ListConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.search.HTMLImageFinder;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.LoopControl;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.SequenceEncoder;
import org.apache.commons.lang3.ArrayUtils;

/**
 * This component places a button into the controls window toolbar.
 * Pressing the button displays a message, plays a sound and/or sends hotkeys */
public class DoActionButton extends AbstractToolbarItem
                            implements RecursionLimiter.Loopable {

  public static final String DO_REPORT = "doReport"; //$NON-NLS-1$
  public static final String REPORT_FORMAT = "reportFormat"; //$NON-NLS-1$
  public static final String DO_SOUND = "doSound"; //$NON-NLS-1$
  public static final String SOUND_CLIP = "soundClip"; //$NON-NLS-1$
  public static final String DO_HOTKEY = "doHotkey"; //$NON-NLS-1$
  public static final String HOTKEYS = "hotkeys"; //$NON-NLS-1$
  public static final String DO_LOOP = "doLoop";  //$NON-NLS-1$
  public static final String LOOP_TYPE = "loopType"; //$NON-NLS-1$
  public static final String LOOP_COUNT = "loopCount"; //$NON-NLS-1$
  public static final String WHILE_EXPRESSION = "whileExpression"; //$NON-NLS-1$
  public static final String UNTIL_EXPRESSION = "untilExpression"; //$NON-NLS-1$
  public static final String PRE_LOOP_HOTKEY = "preLoopKey"; //$NON-NLS-1$
  public static final String POST_LOOP_HOTKEY = "postLoopKey"; //$NON-NLS-1$
  public static final String INDEX = "index"; //$NON-NLS-1$
  public static final String INDEX_PROPERTY = "indexProperty"; //$NON-NLS-1$
  public static final String INDEX_START = "indexStart"; //$NON-NLS-1$
  public static final String INDEX_STEP = "indexStep"; //$NON-NLS-1$

  // These 5 items are identical to those in AbstractToolItem and exist only for "clirr purposes"
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String NAME = "name"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String HOTKEY = "hotkey"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String ICON = "icon"; //$NON-NLS-1$

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launch;

  protected boolean doReport = false;
  protected FormattedString reportFormat =
    new FormattedString(GameModule.getGameModule());
  protected boolean doSound = false;
  protected String soundClip = ""; //$NON-NLS-1$
  protected boolean doHotkey = false;
  protected List<NamedKeyStroke> hotkeys = new ArrayList<>();
  protected boolean doLoop = false;
  protected String loopType = LoopControl.LOOP_COUNTED;
  protected FormattedString loopCount = new FormattedString("1"); //$NON-NLS-1$
  protected PropertyExpression whileExpression = new PropertyExpression();
  protected PropertyExpression untilExpression = new PropertyExpression();
  protected NamedKeyStroke preLoopKey = NamedKeyStroke.NULL_KEYSTROKE;
  protected NamedKeyStroke postLoopKey = NamedKeyStroke.NULL_KEYSTROKE;
  protected boolean hasIndex = false;
  protected String indexProperty = ""; //$NON-NLS-1$
  protected int indexStart = 1;
  protected int indexStep = 1;
  protected int indexValue;

  protected MutableProperty.Impl loopIndexProperty = new MutableProperty.Impl("", this);
  protected boolean loopPropertyRegistered = false;

  public DoActionButton() {
    final ActionListener rollAction = e -> {
      try {
        doActions();
      }
      catch (RecursionLimitException ex) {
        RecursionLimiter.infiniteLoop(ex);
      }
    };

    setLaunchButton(makeLaunchButton(
      getConfigureTypeName(),
      getConfigureTypeName(),
      "",
      rollAction
    ));
    launch = getLaunchButton(); // for compatibility
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.DoAction.component_type"); //$NON-NLS-1$
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(super.getAttributeNames(),
      DO_REPORT,
      REPORT_FORMAT,
      DO_SOUND,
      SOUND_CLIP,
      DO_HOTKEY,
      HOTKEYS,
      DO_LOOP,
      LOOP_TYPE,
      LOOP_COUNT,
      WHILE_EXPRESSION,
      UNTIL_EXPRESSION,
      PRE_LOOP_HOTKEY,
      POST_LOOP_HOTKEY,
      INDEX,
      INDEX_PROPERTY,
      INDEX_START,
      INDEX_STEP
    );
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(super.getAttributeDescriptions(),
      Resources.getString("Editor.DoAction.display_message"), //$NON-NLS-1$
      Resources.getString("Editor.report_format"), //$NON-NLS-1$
      Resources.getString("Editor.DoAction.play_sound"), //$NON-NLS-1$
      Resources.getString("Editor.DoAction.sound_clip"), //$NON-NLS-1$
      Resources.getString("Editor.DoAction.send_hotkeys"), //$NON-NLS-1$
      Resources.getString("Editor.DoAction.hotkeys"), //$NON-NLS-1$
      Resources.getString("Editor.DoAction.repeat_actions"), //$NON-NLS-1$
      Resources.getString("Editor.LoopControl.type_of_loop"), //$NON-NLS-1$
      Resources.getString("Editor.LoopControl.loop_how_many"), //$NON-NLS-1$
      Resources.getString("Editor.LoopControl.looping_continues"), //$NON-NLS-1$
      Resources.getString("Editor.LoopControl.looping_ends"), //$NON-NLS-1$
      Resources.getString("Editor.DoAction.perform_before"), //$NON-NLS-1$
      Resources.getString("Editor.DoAction.perform_after"), //$NON-NLS-1$
      Resources.getString("Editor.LoopControl.loop_index"), //$NON-NLS-1$
      Resources.getString("Editor.LoopControl.index_name"), //$NON-NLS-1$
      Resources.getString("Editor.LoopControl.index_start"), //$NON-NLS-1$
      Resources.getString("Editor.LoopControl.index_step") //$NON-NLS-1$
    );
  }

  @Override
  @SuppressWarnings("unchecked")
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(super.getAttributeTypes(),
      Boolean.class,
      ReportFormatConfig.class,
      Boolean.class,
      SoundConfig.class,
      Boolean.class,
      HotkeyConfig.class,
      Boolean.class,
      LoopConfig.class,
      LoopCountConfig.class,
      PropertyExpression.class,
      PropertyExpression.class,
      NamedKeyStroke.class,
      NamedKeyStroke.class,
      Boolean.class,
      String.class,
      Integer.class,
      Integer.class
    );
  }

  @Deprecated(since = "2020-10-01", forRemoval = true)
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, null);
    }
  }

  public static class SoundConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new AudioClipConfigurer(key, name, GameModule.getGameModule().getArchiveWriter());
    }
  }

  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{});
    }
  }

  public static class HotkeyConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new NamedKeyStrokeArrayConfigurer(key, name, ((DoActionButton) c).hotkeys);
    }
  }

  /**
   * @deprecated not replaced
   */
  @Deprecated (since = "2020-10-21", forRemoval = true)
  public static class NamedHotkeyListConfigurer extends ListConfigurer {
    public NamedHotkeyListConfigurer(String key, String name, List<NamedKeyStroke> list) {
      super(key, name, list);
    }

    @Override
    protected Configurer buildChildConfigurer() {
      return new NamedHotKeyConfigurer(null, Resources.getString(Resources.HOTKEY_LABEL));
    }
  }

  public static class LoopConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new LoopTypeConfig(key, name, ((DoActionButton) c).loopType);
    }
  }

  public static class LoopTypeConfig extends StringEnumConfigurer {
    public LoopTypeConfig(String key, String name, String loopType) {
      super(key, name, LoopControl.LOOP_TYPE_DESCS);
      setValue(LoopControl.loopTypeToDesc(loopType));
    }

    public String[] getValidValues(AutoConfigurable target) {
      return LoopControl.LOOP_TYPE_DESCS;
    }

    @Override
    public String getValueString() {
      return LoopControl.loopDescToType(super.getValueString());
    }
  }

  public static class LoopCountConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedExpressionConfigurer(key, name, ((DoActionButton) c).loopCount);
    }
  }


  @Override
  @SuppressWarnings("unchecked")
  public void setAttribute(String key, Object o) {
    if (DO_REPORT.equals(key)) {
      if (o instanceof String) {
        o = Boolean.valueOf((String) o);
      }
      doReport = (Boolean) o;
    }
    else if (REPORT_FORMAT.equals(key)) {
      reportFormat.setFormat((String) o);
    }
    else if (DO_SOUND.equals(key)) {
      if (o instanceof String) {
        o = Boolean.valueOf((String) o);
      }
      doSound = (Boolean) o;
    }
    if (SOUND_CLIP.equals(key)) {
      if (o instanceof File) {
        o = ((File) o).getName();
      }
      soundClip = (String) o;
    }
    else if (DO_HOTKEY.equals(key)) {
      if (o instanceof String) {
        o = Boolean.valueOf((String) o);
      }
      doHotkey = (Boolean) o;
    }
    else if (HOTKEYS.equals(key)) {
      if (o instanceof String) {
        o = decodeHotkeys((String) o);
      }
      else if (o instanceof NamedKeyStroke[]) {
        o = Arrays.asList((NamedKeyStroke[]) o);
      }
      hotkeys = (List<NamedKeyStroke>) o;
    }
    else if (DO_LOOP.equals(key)) {
      if (o instanceof String) {
        o = Boolean.valueOf((String) o);
      }
      doLoop = (Boolean) o;
      updateLoopPropertyRegistration();
    }
    else if (LOOP_TYPE.equals(key)) {
      loopType = LoopControl.loopDescToType((String) o);
    }
    else if (LOOP_COUNT.equals(key)) {
      loopCount.setFormat((String) o);
    }
    else if (WHILE_EXPRESSION.equals(key)) {
      whileExpression.setExpression((String) o);
    }
    else if (UNTIL_EXPRESSION.equals(key)) {
      untilExpression.setExpression((String) o);
    }
    else if (PRE_LOOP_HOTKEY.equals(key)) {
      if (o instanceof String) {
        o = NamedHotKeyConfigurer.decode((String) o);
      }
      preLoopKey = (NamedKeyStroke) o;
    }
    else if (POST_LOOP_HOTKEY.equals(key)) {
      if (o instanceof String) {
        o = NamedHotKeyConfigurer.decode((String) o);
      }
      postLoopKey = (NamedKeyStroke) o;
    }
    else if (INDEX.equals(key)) {
      if (o instanceof String) {
        o = Boolean.valueOf((String) o);
      }
      hasIndex = (Boolean) o;
      updateLoopPropertyRegistration();
    }
    else if (INDEX_PROPERTY.equals(key)) {
      indexProperty = (String) o;
      loopIndexProperty.setPropertyName(indexProperty);
      updateLoopPropertyRegistration();
    }
    else if (INDEX_START.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      indexStart = (Integer) o;
    }
    else if (INDEX_STEP.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      indexStep = (Integer) o;
    }
    else {
      super.setAttribute(key, o);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (DO_REPORT.equals(key)) {
      return String.valueOf(doReport);
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat.getFormat();
    }
    else if (DO_SOUND.equals(key)) {
      return String.valueOf(doSound);
    }
    else if (SOUND_CLIP.equals(key)) {
      return soundClip;
    }
    else if (DO_HOTKEY.equals(key)) {
      return String.valueOf(doHotkey);
    }
    else if (HOTKEYS.equals(key)) {
      return encodeHotkeys();
    }
    else if (DO_LOOP.equals(key)) {
      return String.valueOf(doLoop);
    }
    else if (LOOP_TYPE.equals(key)) {
      return loopType;
    }
    else if (LOOP_COUNT.equals(key)) {
      return loopCount.getFormat();
    }
    else if (WHILE_EXPRESSION.equals(key)) {
      return whileExpression.getExpression();
    }
    else if (UNTIL_EXPRESSION.equals(key)) {
      return untilExpression.getExpression();
    }
    else if (PRE_LOOP_HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(preLoopKey);
    }
    else if (POST_LOOP_HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(postLoopKey);
    }
    else if (INDEX.equals(key)) {
      return String.valueOf(hasIndex);
    }
    else if (INDEX_PROPERTY.equals(key)) {
      return indexProperty;
    }
    else if (INDEX_START.equals(key)) {
      return String.valueOf(indexStart);
    }
    else if (INDEX_STEP.equals(key)) {
      return String.valueOf(indexStep);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (REPORT_FORMAT.equals(name)) {
      return () -> doReport;
    }
    else if (SOUND_CLIP.equals(name)) {
      return () -> doSound;
    }
    else if (HOTKEYS.equals(name)) {
      return () -> doHotkey;
    }
    else if (LOOP_COUNT.equals(name)) {
      return () -> doLoop && LoopControl.LOOP_COUNTED.equals(loopType);
    }
    else if (WHILE_EXPRESSION.equals(name)) {
      return () -> doLoop && LoopControl.LOOP_WHILE.equals(loopType);
    }
    else if (UNTIL_EXPRESSION.equals(name)) {
      return () -> doLoop && LoopControl.LOOP_UNTIL.equals(loopType);
    }
    else if (List.of(LOOP_TYPE, PRE_LOOP_HOTKEY, POST_LOOP_HOTKEY, INDEX).contains(name)) {
      return () -> doLoop;
    }
    else if (List.of(INDEX_PROPERTY, INDEX_START, INDEX_STEP).contains(name)) {
      return () -> doLoop && hasIndex;
    }
    else {
      return null;
    }
  }

  protected String encodeHotkeys() {
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final NamedKeyStroke key : hotkeys) {
      se.append(NamedHotKeyConfigurer.encode(key));
    }

    final String val = se.getValue();
    return val == null ? "" : val; //$NON-NLS-1$
  }

  protected List<NamedKeyStroke> decodeHotkeys(String s) {
    final List<NamedKeyStroke> list = new ArrayList<>();
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    while (sd.hasMoreTokens()) {
      final NamedKeyStroke key = NamedHotKeyConfigurer.decode(sd.nextToken());
      list.add(key);
    }
    return list;
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }


  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("DoActionButton.html"); //$NON-NLS-1$
  }

  /**
   * Register/Deregister the Global Property exposing the index property. It
   * is only visible if looping is turned on and an Index Property is specified
   */
  protected void updateLoopPropertyRegistration() {
    final boolean shouldBeRegistered = doLoop && hasIndex && indexProperty.length() > 0;
    if (shouldBeRegistered && !loopPropertyRegistered) {
      loopIndexProperty.addTo(GameModule.getGameModule());
      loopPropertyRegistered = true;
    }
    else if (!shouldBeRegistered && loopPropertyRegistered) {
      loopIndexProperty.removeFromContainer();
      loopPropertyRegistered = false;
    }
  }

  protected void setIndexPropertyValue() {
    loopIndexProperty.setPropertyValue(String.valueOf(indexValue));
  }

  protected void doActions() throws RecursionLimitException {
    final Command c = new NullCommand();
    final GameModule mod = GameModule.getGameModule();

    // Non looping case
    if (! doLoop) {
      executeActions(c);
      mod.sendAndLog(c);
      return;
    }

    // Set up Index Property
    indexValue = indexStart;
    setIndexPropertyValue();

    // Issue the Pre-loop key
    doHotKey(c, preLoopKey);

    // Set up counters for a counted loop
    int loopCounter = 0;
    int loopCountLimit = 0;
    if (LoopControl.LOOP_COUNTED.equals(loopType)) {
      loopCountLimit = loopCount.getTextAsInt(mod, Resources.getString("Editor.LoopControl.loop_count"), this); //$NON-NLS-1$
    }

    RecursionLimitException loopException = null;

    for (;;) {

      // While loop - test condition is still true before actions
      if (LoopControl.LOOP_WHILE.equals(loopType)) {
        if (!whileExpression.isTrue(mod)) {
          break;
        }
      }

      // Execute the actions and catch and looping. Save any
      // loop Exception to be thrown after the post-loop code
     // to ensure post-loop key is executed.
      try {
        executeActions(c);
      }
      catch (RecursionLimitException ex) {
        loopException = ex;
        break;
      }

      // Until loop - test condition is not false after loop
      if (LoopControl.LOOP_UNTIL.equals(loopType)) {
        if (untilExpression.isTrue(mod)) {
          break;
        }
      }

      // Counted loop - Check if looped enough times
      loopCounter++;
      if (LoopControl.LOOP_COUNTED.equals(loopType)) {
        if (loopCounter >= loopCountLimit) {
          break;
        }
      }
      // Otherwise check for too much looping.
      else {
        if (loopCounter >= LoopControl.LOOP_LIMIT) {
          loopException = new RecursionLimitException(this);
          break;
        }
      }

      // Increment the Index Variable
      indexValue += indexStep;
      setIndexPropertyValue();
    }

    // Issue the Post-loop key
    doHotKey(c, postLoopKey);

    // Send the accumulated commands to the log
    mod.sendAndLog(c);

    // If the loop ended due to excessive looping, throw the
    // Exception out to the caller.
    if (loopException != null) {
      throw loopException;
    }
  }

  /**
   * Execute the set of actions that make up this button and
   * return the set of Commands generated.
   *
   * @param command command to execute
   * @throws RecursionLimitException recursion protection
   */
  protected void executeActions(Command command) throws RecursionLimitException {
    final GameModule mod = GameModule.getGameModule();

    // GameModule.pauseLogging() returns false if logging is already paused by
    // a higher level component.
    final boolean loggingPaused = mod.pauseLogging();

    try {
      RecursionLimiter.startExecution(this);
      if (doReport) {
        final String report = "* " + reportFormat.getLocalizedText(); //$NON-NLS-1$
        final Command c = new Chatter.DisplayText(mod.getChatter(), report);
        c.execute();
        mod.sendAndLog(c);
      }

      if (doSound) {
        final String clipName = new FormattedString(soundClip).getText(mod);
        final Command c = new PlayAudioClipCommand(clipName);
        c.execute();
        mod.sendAndLog(c);
      }

      // Send the hotkeys. Individual hotkeys have already executed
      // the commands they generated.
      if (doHotkey) {
        for (final NamedKeyStroke key : hotkeys) {
          mod.fireKeyStroke(key);
        }
      }
    }
    finally {
      RecursionLimiter.endExecution();
      // If we paused the log, then retrieve the accumulated commands
      // generated by all actions and restart logging.
      if (loggingPaused) {
        command.append(mod.resumeLogging());
      }
    }
  }

  // Perform an individual Hotkey and return any generated commands
  // if logging has not already been paused.
  protected void doHotKey(Command c, NamedKeyStroke key) {
    if (!key.isNull()) {
      final GameModule mod = GameModule.getGameModule();
      final boolean loggingPaused = mod.pauseLogging();
      try {
        mod.fireKeyStroke(key);
      }
      finally {
        if (loggingPaused) {
          c.append(mod.resumeLogging());
        }
      }
    }
  }

  // Implement Loopable
  @Override
  public String getComponentTypeName() {
    return getConfigureTypeName();
  }

  @Override
  public String getComponentName() {
    return getConfigureName();
  }

  /**
   * Implement PropertyNameSource - Expose loop index property if looping turned on
   */
  @Override
  public List<String> getPropertyNames() {
    if (doLoop && hasIndex) {
      final ArrayList<String> l = new ArrayList<>();
      l.add(indexProperty);
      return l;
    }
    else {
      return super.getPropertyNames();
    }
  }

  /**
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    final List<String> l = new ArrayList<>();
    if (doLoop) {
      if (LoopControl.LOOP_WHILE.equals(loopType)) {
        l.add(whileExpression.getExpression());
      }
      else if (LoopControl.LOOP_UNTIL.equals(loopType)) {
        l.add(untilExpression.getExpression());
      }
      else if (LoopControl.LOOP_COUNTED.equals(loopType)) {
        l.add(loopCount.getFormat());
      }
    }
    return l;
  }

  /**
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(reportFormat.getFormat());
  }

  /**
   * @return a list of any Property Names referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return List.of(indexProperty);
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    final List<NamedKeyStroke> l = new ArrayList<>(super.getNamedKeyStrokeList());
    Collections.addAll(l, preLoopKey, postLoopKey);
    return l;
  }

  /**
   * In case reports use HTML and  refer to any image files
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    final HTMLImageFinder h = new HTMLImageFinder(reportFormat.getFormat());
    h.addImageNames(s);
  }
}
