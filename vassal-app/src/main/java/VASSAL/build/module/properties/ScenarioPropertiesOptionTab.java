/*
 * Copyright (c) 2023 by The VASSAL Development Team
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
package VASSAL.build.module.properties;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AbstractFolder;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.ComponentConfigPanel;
import VASSAL.configure.Configurer;
import VASSAL.configure.NotNullConfigureName;
import VASSAL.configure.PlayerIdFormattedExpressionConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.SwingUtils;

import net.miginfocom.swing.MigLayout;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import java.awt.Component;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class ScenarioPropertiesOptionTab extends AbstractConfigurable implements MutablePropertiesContainer, CommandEncoder, GameComponent {

  public static final char COMMAND_DELIMITER = '\t';
  public static final String COMMAND_PREFIX = "LOCKSPOTAB";

  public static final String NAME = "name"; // NON-NLS
  public static final String DESCRIPTION = "description"; // NON-NLS
  public static final String REPORT = "report"; // NON-NLS

  public static final String REPORT_PROP_NAME = "propertyName"; // NON-NLS
  public static final String REPORT_OLD_VALUE = "oldValue"; // NON-NLS
  public static final String REPORT_NEW_VALUE = "newValue"; // NON-NLS
  public static final String REPORT_PROMPT = "propertyPrompt"; // NON-NLS
  public static final String REPORT_TAB = "tabName"; // NON-NLS

  protected String description = "";
  protected MutablePropertiesContainer parentContainer;
  protected Buildable parent; // Since we delegate all the action up to the ScenarioOptions component
  protected FormattedString reportFormat = new FormattedString(Resources.getString("ScenarioOptions.default_report"));

  /** Username that locked this tab **/
  protected String lockedByuser = "";

  /** Password of User that locked this tab **/
  protected String lockedBypw = "";

  /** UTD Date/Time tab was locked */
  protected String lockedDt;

  /** Current locked status of the tab recorded in the UI, but not yet saved **/
  protected boolean uiTabLock;

  /** User who locked the UI **/
  protected String uiTabLockUser;

  /** UTC Date/Time of lock */
  protected String uiTabLockDt;

  protected JLabel lockLabel;
  protected JButton lockButton;
  protected JLabel unlockLabel;
  protected JButton unlockButton;

  protected ComponentConfigPanel uiPanel = new ComponentConfigPanel();
  protected Map<String, Entry> entries = new HashMap<>();

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ScenarioPropertiesTab.component_type"); //$NON-NLS-1$
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{NAME, DESCRIPTION, REPORT};
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String) value;
    }
    else if (REPORT.equals(key)) {
      reportFormat.setFormat((String) value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (DESCRIPTION.equals(key)) {
      return description;
    }
    else if (REPORT.equals(key)) {
      return reportFormat.getFormat();
    }
    return null;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.NAME_LABEL),
      Resources.getString(Resources.DESCRIPTION),
      Resources.getString(Resources.REPORT_FORMAT_LABEL)
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class[]{String.class, String.class, ChangeOptionConfig.class};
  }

  @Override
  public void addTo(Buildable parent) {

    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder) parent).getNonFolderAncestor();
    }

    this.parent = parent;
    parentContainer = (MutablePropertiesContainer) parent;
    validator = new NotNullConfigureName(this);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  public Buildable getParent() {
    return parent;
  }

  public FormattedString getReportFormat() {
    return reportFormat;
  }

  @Override
  public void removeFrom(Buildable parent) {
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{ListScenarioProperty.class, BooleanScenarioProperty.class, StringScenarioProperty.class, NumberScenarioProperty.class};
  }

  @Override
  public void addMutableProperty(String key, MutableProperty p) {
    parentContainer.addMutableProperty(key, p);
  }

  @Override
  public MutableProperty removeMutableProperty(String key) {
    return parentContainer.removeMutableProperty(key);
  }

  @Override
  public MutableProperty getMutableProperty(String propertyName) {
    return parentContainer.getMutableProperty(propertyName);
  }

  @Override
  public String getMutablePropertiesContainerId() {
    return parentContainer.getMutablePropertiesContainerId();
  }

  @Override
  public Command decode(String command) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(command, COMMAND_DELIMITER);

    final String prefix = sd.nextToken("");
    if (!(COMMAND_PREFIX).equals(prefix)) {
      return null;
    }

    final String name = sd.nextToken("");
    if (! name.equals(getConfigureName())) {
      return null;
    }

    final String by = sd.nextToken("");
    final String pw = sd.nextToken("");
    final String dt = sd.nextToken("");

    return new LockScenarioOptionsTab(this, by, pw, dt);
  }

  @Override
  public String encode(Command c) {
    if (c instanceof LockScenarioOptionsTab) {
      final LockScenarioOptionsTab com = (LockScenarioOptionsTab) c;
      final SequenceEncoder se = new SequenceEncoder(COMMAND_PREFIX, COMMAND_DELIMITER);
      se.append(com.getTab().getConfigureName())
        .append(com.getLockedBy())
        .append(com.getLockedPw())
        .append(com.getLockedDt());
      return se.getValue();
    }
    return null;
  }

  @Override
  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      resetTab();
    }
  }

  protected void resetTab() {
    lockedByuser = "";
    lockedBypw = "";
    lockedDt = "";
    uiTabLock = false;
    uiTabLockUser = "";
    uiTabLockDt = "";
  }

  @Override
  public Command getRestoreCommand() {
    return new LockScenarioOptionsTab(this, lockedByuser, lockedBypw, lockedDt);
  }

  // Return the UI to manipulate the options in this tab
  public Component getUI() {
    uiTabLock = isLocked();
    uiTabLockUser = lockedByuser;
    uiTabLockDt = lockedDt;
    rebuildUI();
    return uiPanel;
  }

  protected void rebuildUI() {
    entries.clear();
    uiPanel.removeAll();
    uiPanel.add(getLockUI(), "span 2, alignx left"); // NON-NLS

    // Loop through each option defined option
    for (final AbstractScenarioProperty option : getAllDescendantComponentsOf(AbstractScenarioProperty.class)) {

      // Save the option for processing after OK is clicked
      final Entry entry = new Entry(option);
      entries.put(option.getConfigureName(), entry);

      // Add the option configurer to the tab
      uiPanel.add(new JLabel(entry.getDescription()), entry.getConfigurer());

    }
    refreshLockUI();
  }

  /**
   * Return a UI that displays and manipulates the Locked status of this tab.
   * The UI will be different for different users.
   * a) Tab not locked - show a lock button
   * b) Tab locked by me - show an unlock button
   * c) Tab locked by other, just show
   *
   * @return generated UI
   */
  public Component getLockUI() {
    final JPanel p = new JPanel();
    p.setBorder(BorderFactory.createEtchedBorder());
    p.setLayout(new MigLayout("ins 5, hidemode 3", "[]10[]")); // NON-NLS

    lockLabel = new JLabel(Resources.getString("ScenarioProperties.tabUnlocked"));
    p.add(lockLabel);
    lockButton = new JButton(Resources.getString("ScenarioProperties.lock"));
    lockButton.addActionListener(e -> lockUi(true));
    p.add(lockButton);

    unlockLabel = new JLabel(Resources.getString("ScenarioProperties.tabLocked", uiTabLockUser, uiTabLockDt));
    p.add(unlockLabel);
    unlockButton = new JButton(Resources.getString("ScenarioProperties.unlock"));
    unlockButton.addActionListener(e -> lockUi(false));
    p.add(unlockButton);

    refreshLockUI();
    return p;
  }

  /**
   * Update the locked status of the UI for this tab. The underlying tab will not be locked
   * until user clicks OK on the enclosing dialog.
   *
   * @param lock lock status
   */
  protected void lockUi(boolean lock) {
    final StringBuilder sb = new StringBuilder();
    sb.append(Resources.getString(lock ? "ScenarioProperties.lock_messaage" : "ScenarioProperties.unlock_messaage"))
      .append("\n\n")
      .append(Resources.getString("ScenarioProperties.are_you_sure"))
      .append("\n\n");

    if (JOptionPane.YES_OPTION ==
      JOptionPane.showConfirmDialog(uiPanel, sb.toString(), Resources.getString("ScenarioProperties.lock_title"), JOptionPane.YES_NO_OPTION)) {
      uiTabLock = lock;
      uiTabLockUser = (String) GameModule.getGameModule().getPrefs().getValue(GameModule.REAL_NAME);
      final Instant now = Instant.now();
      final DateTimeFormatter fmt = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM, FormatStyle.MEDIUM);
      final String local = fmt.withZone(ZoneId.systemDefault()).format(now);
      final String utc = fmt.withZone(ZoneOffset.UTC).format(now);
      uiTabLockDt = local + " (" + utc + " UTC)";
      refreshLockUI();
    }
  }

  /**
   * Refresh the accessibility of all UI components
   */
  protected void refreshLockUI() {

    lockLabel.setVisible(!isUiLocked());
    lockButton.setVisible(!isUiLocked());
    lockButton.setEnabled(isLockingAllowed());

    unlockLabel.setText(Resources.getString("ScenarioProperties.tabLocked", uiTabLockUser, uiTabLockDt));
    unlockLabel.setVisible(isUiLocked());
    unlockButton.setVisible(isUiLocked());
    unlockButton.setEnabled(isLockingAllowed());

    // Individual options are only enabled if
    // the tab has not been locked AND
    // the user is allowed to lock the UI if unlocked (non observer with name and password)
    for (final Entry entry : entries.values()) {
      entry.setEnabled(!isUiLocked() && isLockingAllowed());
    }

    SwingUtils.repack(uiPanel);
  }


  /**
   * Lock or unlock the tab.
   *
   * @param locking true to lock
   */
  protected void lockTab(boolean locking, String lockDt) {

    final GameModule gm = GameModule.getGameModule();
    final String user = (String) gm.getPrefs().getValue(GameModule.REAL_NAME);
    final String pw = (String) gm.getPrefs().getValue(GameModule.SECRET_NAME);

    Command c;
    if (locking) {
      c = new LockScenarioOptionsTab(this, user, pw, lockDt);
      c = c.append(new Chatter.DisplayText(gm.getChatter(), Resources.getString("ScenarioProperties.lockReport", getConfigureName(), user)));
    }
    else {
      c = new LockScenarioOptionsTab(this, "", "", "");
      c = c.append(new Chatter.DisplayText(gm.getChatter(), Resources.getString("ScenarioProperties.unlockReport", getConfigureName(), user)));
    }

    c.execute();
    gm.sendAndLog(c);

  }

  /**
   * Is the current user allowed to change the locked state of a Scenario Option tab
   *  - A Locked tab can only be unlocked by the user who locked it.
   *  - An unlocked tab can only be locked by
   *     o A user with a real name specified
   *     o A user with a password specified
   *     o If Player sides are defined, then the player must be one of the defined sides.
   *
   * @return true if action allowed.
   */
  protected boolean isLockingAllowed() {

    final GameModule gm = GameModule.getGameModule();
    final String pw = (String) gm.getPrefs().getValue(GameModule.SECRET_NAME);

    if (isUiLocked()) {
      return getLockedBypw().equals(pw);
    }
    else {

      // Locking player must have a username specified
      if (!gm.isRealName()) {
        return false;
      }

      // Locking player must have a password specified
      if (!gm.isNonBlankPassword()) {
        return false;
      }

      // Observers can only lock if no player sides defined
      final PlayerRoster r = gm.getPlayerRoster();
      if (r == null) {
        return true;
      }

      // Otherwise, player must have a side to be able to lock
      // Note side=null means no player sides defined, so Ok to lock
      final String side = PlayerRoster.getMySide();
      return !PlayerRoster.OBSERVER.equals(side);
    }
  }

  public String getLockedByuser() {
    return lockedByuser;
  }

  public String getLockedBypw() {
    return lockedBypw;
  }

  public static class ChangeOptionConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedExpressionConfigurer(key, name, new String[]{
        REPORT_PROP_NAME,
        REPORT_PROMPT,
        REPORT_OLD_VALUE,
        REPORT_NEW_VALUE,
        REPORT_TAB});
    }
  }

  /** Is this Options tab currently locked? **/
  public boolean isLocked() {
    return ! lockedByuser.isBlank();
  }

  /** Is this options tab recorded as locked in the UI? **/
  public boolean isUiLocked() {
    return uiTabLock;
  }

  /**
   * Process any changes made to this Tab and it's options. Called when OK is pressed
   * on the main Options dialog.
   * Logging has been paused by the level above, so we can just generate and log Commands as
   * required. They will be grouped into a single undo at the level above.
   *
   */
  public void processChanges() {

    // Process each Option
    for (final AbstractScenarioProperty property : getAllDescendantComponentsOf(AbstractScenarioProperty.class)) {
      final Entry entry = entries.get(property.getConfigureName());
      if (entry.hasValueChanged()) {
        entry.getProperty().processOptionChange(entry.getNewValue());
      }
    }

    if (isLocked() != uiTabLock) {
      lockTab(uiTabLock, uiTabLockDt);
    }

  }

  /**
   * A Command to record the lock status of an individual Scenario Options tab
   */
  public static class LockScenarioOptionsTab extends Command {

    private final ScenarioPropertiesOptionTab tab;

    private final String lockedBy;
    private final String lockedPw;
    private final String lockedDt;
    private final String oldLockedBy;
    private final String oldLockedPw;
    private final String oldLockedDt;

    public LockScenarioOptionsTab(ScenarioPropertiesOptionTab tab, String lockedBy, String lockedPw, String lockedDt) {
      this.tab = tab;
      this.lockedBy = lockedBy;
      this.lockedPw = lockedPw;
      this.lockedDt = lockedDt;
      oldLockedBy = tab.lockedByuser;
      oldLockedPw = tab.lockedBypw;
      oldLockedDt = tab.lockedDt;
    }

    @Override
    protected void executeCommand() {
      tab.lockedByuser = lockedBy;
      tab.lockedBypw = lockedPw;
      tab.lockedDt = lockedDt;
    }

    @Override
    protected Command myUndoCommand() {
      return new LockScenarioOptionsTab(tab, oldLockedBy, oldLockedPw, oldLockedDt);
    }

    public ScenarioPropertiesOptionTab getTab() {
      return tab;
    }

    public String getLockedBy() {
      return lockedBy;
    }

    public String getLockedPw() {
      return lockedPw;
    }

    public String getLockedDt() {
      return lockedDt;
    }
  }

  /**
   * Hold an option and the generated Configurer for this current opening of the options window
   */
  private static class Entry {
    private final AbstractScenarioProperty property;
    private final String oldValue;
    private final Configurer configurer;

    public Entry(AbstractScenarioProperty property) {
      this.property = property;
      this.oldValue = property.getPropertyValue();
      this.configurer = property.getOptionConfigurer();
      this.configurer.getControls();
    }

    public boolean hasValueChanged() {
      return !Objects.equals(oldValue, getNewValue());
    }

    public String getOldValue() {
      return oldValue;
    }

    public String getNewValue() {
      return configurer.getValueString();
    }

    public Configurer getConfigurer() {
      return configurer;
    }

    public String getDescription() {
      return property.getDescription();
    }

    public AbstractScenarioProperty getProperty() {
      return property;
    }

    public void setEnabled(boolean enabled) {
      getConfigurer().setEnabled(enabled);
    }
  }
}
