/*
 * $Id$
 *
 * Copyright (c) 2004 by Michael Blumohr, Rodney Kinney
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.io.IOException;
import java.util.ArrayList;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.KeyStroke;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.KeyStrokeListener;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;


/**
 * ...
 */
// TODO Expose result as property
public class SpecialDiceButton extends AbstractConfigurable implements CommandEncoder, UniqueIdManager.Identifyable {
  protected static UniqueIdManager idMgr = new UniqueIdManager("SpecialDiceButton"); //$NON-NLS-1$
  public static final String SHOW_RESULTS_COMMAND = "SHOW_RESULTS\t"; //$NON-NLS-1$

  protected ArrayList<SpecialDie> dice = new ArrayList<SpecialDie>();
  protected java.util.Random ran;
  protected boolean reportResultAsText = true;
  protected boolean reportResultInWindow = false;
  protected boolean reportResultInButton = false;
  private LaunchButton launch;
  protected String id;
  protected String sMapName;

  protected JDialog dialog; // Dialog to show results graphical
  protected JLabel dialogLabel;
  protected Color bgColor;
  protected ResultsIcon resultsIcon = new ResultsIcon();

  protected FormattedString format = new FormattedString();
  protected String chatResultFormat = "** $" + NAME + "$ = [$result1$] *** <$" + GlobalOptions.PLAYER_NAME + "$>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  protected String windowTitleResultFormat = "$" + NAME + "$"; //$NON-NLS-1$ //$NON-NLS-2$
  protected String tooltip = ""; //$NON-NLS-1$

  public static final String BUTTON_TEXT = "text";
  public static final String TOOLTIP = "tooltip";
  public static final String NAME = "name";
  public static final String ICON = "icon";
  public static final String RESULT_CHATTER = "resultChatter";
  public static final String CHAT_RESULT_FORMAT = "format";
  public static final String RESULT_N = "result#";
  public static final String RESULT_TOTAL = "numericalTotal";
  public static final String RESULT_WINDOW = "resultWindow";
  public static final String WINDOW_TITLE_RESULT_FORMAT = "windowTitleResultFormat";
  public static final String RESULT_BUTTON = "resultButton";
  public static final String WINDOW_X = "windowX";
  public static final String WINDOW_Y = "windowY";
  public static final String BACKGROUND_COLOR = "backgroundColor";
  public static final String DICE_SET = "diceSet";
  public static final String HOTKEY = "hotkey";
  public static final String NONE = "<none>";

  private static final int[] EMPTY = new int[0];

  public SpecialDiceButton() {
    dialog = new JDialog(GameModule.getGameModule().getFrame());
    dialogLabel = new JLabel();
    dialogLabel.setIcon(resultsIcon);
    dialog.getContentPane().add(dialogLabel);

    ActionListener rollAction = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        DR();
      }
    };
    launch = new LaunchButton(null, TOOLTIP, BUTTON_TEXT, HOTKEY, ICON, rollAction);
    setAttribute(NAME, "Symbols");
    setAttribute(BUTTON_TEXT, "Sym");
    launch.setAttribute(TOOLTIP, "Symbols");
  }

  public static String getConfigureTypeName() {
    return "Symbolic Dice Button";
  }

  /**
   * The text reported before the results of the roll
   */
  protected String getReportPrefix() {
    return " *** " + getConfigureName() + " = "; //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * The text reported after the results of the roll;
   * @deprecated
   */
  protected String getReportSuffix() {
    return " ***  <" //$NON-NLS-1$
        + GameModule.getGameModule().getChatter().getHandle() + ">"; //$NON-NLS-1$
  }

  /**
   * Forwards the result of the roll to the {@link Chatter#send}
   * method of the {@link Chatter} of the {@link GameModule}.  Format is
   * prefix+[comma-separated roll list]+suffix
   * additionally a command for every die is generated
   */
  protected void DR() {
    int[] results = new int[dice.size()];
    int i = 0;
    for (SpecialDie sd : dice) {
      results[i++] = ran.nextInt(sd.getFaceCount());
    }
    Command c = reportResults(results);
    if (reportResultAsText) {
      c = c.append(reportTextResults(results));
    }
    GameModule.getGameModule().sendAndLog(c);
  }

  private Command reportResults(int[] results) {
    resultsIcon.setResults(results);

    if (reportResultInWindow) {
      dialog.setVisible(true);
      format.setFormat(windowTitleResultFormat);
      dialog.setTitle(format.getLocalizedText());
      dialogLabel.repaint();
    }
    if (reportResultInButton) {
      launch.repaint();
    }
    return new ShowResults(this, results);
  }

  private Command reportTextResults(int[] results) {
    format.setProperty(NAME, getLocalizedConfigureName());

    int total = 0;
    for (int i = 0; i < dice.size(); ++i) {
      SpecialDie die = dice.get(i);
      format.setProperty("result" + (i + 1), die.getTextValue(results[i])); //$NON-NLS-1$
      total += die.getIntValue(results[i]);
    }
    format.setProperty(RESULT_TOTAL, "" + total); //$NON-NLS-1$
    format.setFormat(chatResultFormat);
    String msg = format.getLocalizedText();
    if (msg.startsWith("*")) { //$NON-NLS-1$
      msg = "*" + msg; //$NON-NLS-1$
    }
    else {
      msg = "* " + msg; //$NON-NLS-1$
    }
    Command c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), msg);
    c.execute();
    return c;
  }

  /**
   * The Attributes of a DiceButton are:
   *
   * <code>BUTTON_TEXT</code> the label of the button in the toolbar
   * <code>ICON</code> the icon of the button in the toolbar
   * <code>HOTKEY</code> the hotkey equivalent of the button
   * <code>DICE_SET</code> list of dice sets, an entry can be:
   *                       [number]name of die[+|-modifier]
   *                       "name of die" must be SpecialDie
   *                       "modifier" is added/subtracted to/from total of dice
   *                       [number]Dnumber of sides (e.g. 2D6)
   * <code>NUMERIC</code> result of all dice is numeric
   * <code>REPORT_TOTAL</code> If numeric and true, add the results of the dice together and report the total.  Otherwise, report the individual results
   * <code>SORT</code> if true sort results per die by numeric value
   * <code>RESULT_CHATTER</code> if true report results in chatter
   * <code>RESULT_WINDOW</code> if true show result graphical in extra window
   * <code>WINDOW_X</code> width of window or button
   * <code>WINDOW_Y</code> height of window or button
   * <code>RESULT_MAP</code> :TODO: if true show result in special area in map
   * <code>MAP_NAME</code> :TODO: name of map
   * <code>RESULT_BUTTON</code> if true show result graphical in button
   */
  public String[] getAttributeNames() {
    String s[] = {NAME, BUTTON_TEXT, TOOLTIP, ICON, HOTKEY,
                  RESULT_CHATTER, CHAT_RESULT_FORMAT,
                  RESULT_WINDOW, WINDOW_TITLE_RESULT_FORMAT,
                  RESULT_BUTTON, WINDOW_X, WINDOW_Y, BACKGROUND_COLOR};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name:  ",
                        "Button text:  ",
                        "Tooltip text:  ",
                        "Button icon:  ",
                        "Hotkey:  ",
                        "Report results as text?",
                        "Report format:  ",
                        "Show result in window?",
                        "Window title format:  ",
                        "Show result in button?",
                        "Width:  ",
                        "Heidght:  ",
                        "Background color:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class,
                       String.class,
                       String.class,
                       IconConfig.class,
                       KeyStroke.class,
                       Boolean.class,
                       ReportFormatConfig.class,
                       Boolean.class,
                       ReportFormatConfig.class,
                       Boolean.class,
                       Integer.class,
                       Integer.class,
                       Color.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/die.gif"); //$NON-NLS-1$
    }
  }

  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{NAME, RESULT_N, RESULT_TOTAL});
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {
// get size only when output in window or on button
    if (WINDOW_X.equals(name)
        || WINDOW_Y.equals(name)
        || BACKGROUND_COLOR.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return reportResultInWindow || reportResultInButton;
        }
      };
    }
    else if (CHAT_RESULT_FORMAT.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return reportResultAsText;
        }
      };
    }
    else if (WINDOW_TITLE_RESULT_FORMAT.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return reportResultInWindow;
        }
      };
    }
    else
      return null;
  }

  public void addSpecialDie(SpecialDie d) {
    dice.add(d);
  }

  public void removeSpecialDie(SpecialDie d) {
    dice.remove(d);
  }

  /**
   * Expects to be added to a SymbolDice.  Adds the button to the
   * control window's toolbar and registers itself as a {@link
   * KeyStrokeListener} */
  public void addTo(Buildable parent) {
    resultsIcon.setResults(new int[dice.size()]);

    launch.addHierarchyListener(new HierarchyListener() {
      public void hierarchyChanged(HierarchyEvent e) {
        if (launch.isShowing()) {
          dialog.setLocationRelativeTo(launch);
          launch.removeHierarchyListener(this);
        }
      }
    });


    GameModule mod = GameModule.getGameModule();
    ran = mod.getRNG();

    mod.getToolBar().add(launch);

    idMgr.add(this);

    mod.addCommandEncoder(this);
  }


  public void removeFrom(Buildable b) {
    GameModule mod = GameModule.getGameModule();
    mod.removeCommandEncoder(this);

    mod.getToolBar().remove(launch);
    mod.getToolBar().revalidate();
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  /**
   * Make a best gues for a unique identifier for the target.
   * Use {@link VASSAL.tools.UniqueIdManager.Identifyable#getConfigureName if non-null, otherwise
   * use {@link VASSAL.tools.UniqueIdManager.Identifyable#getId
   * @param target
   * @return
   */
  public String getIdentifier() {
    return UniqueIdManager.getIdentifier(this);
  }

  /**
   * get boolean value of object
   * @param o object as input for setAttribute()
   * @return  boolean value of object
   */
  private boolean getBoolVal(Object o) {
    if (o instanceof Boolean) {
      boolean bool = ((Boolean) o).booleanValue();
      return bool;
    }
    else if (o instanceof String) {
      return "true".equals(o); //$NON-NLS-1$
    }
    else
      return false;
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
      launch.setToolTipText((String) o);
    }
    else if (RESULT_CHATTER.equals(key)) {
      reportResultAsText = getBoolVal(o);
    }
    else if (CHAT_RESULT_FORMAT.equals(key)) {
      chatResultFormat = (String) o;
    }
    else if (RESULT_BUTTON.equals(key)) {
      reportResultInButton = getBoolVal(o);
      if (reportResultInButton) {
        launch.setIcon(resultsIcon);
      }
    }
    else if (RESULT_WINDOW.equals(key)) {
      reportResultInWindow = getBoolVal(o);
    }
    else if (WINDOW_TITLE_RESULT_FORMAT.equals(key)) {
      windowTitleResultFormat = (String) o;
    }
    else if (WINDOW_X.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      resultsIcon.width = ((Integer) o).intValue();
      dialog.pack();
    }
    else if (WINDOW_Y.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      resultsIcon.height = ((Integer) o).intValue();
      dialog.pack();
    }
    else if (BACKGROUND_COLOR.equals(key)) {
      if (o instanceof String) {
        o = ColorConfigurer.stringToColor((String) o);
      }
      bgColor = (Color) o;
    }
    else if (TOOLTIP.equals(key)) {
      tooltip = (String) o;
      launch.setAttribute(key, o);
    }
    else {
      launch.setAttribute(key, o);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (RESULT_CHATTER.equals(key)) {
      return "" + reportResultAsText; //$NON-NLS-1$
    }
    else if (CHAT_RESULT_FORMAT.equals(key)) {
      return chatResultFormat;
    }
    else if (RESULT_BUTTON.equals(key)) {
      return "" + reportResultInButton; //$NON-NLS-1$
    }
    else if (RESULT_WINDOW.equals(key)) {
      return "" + reportResultInWindow; //$NON-NLS-1$
    }
    else if (WINDOW_TITLE_RESULT_FORMAT.equals(key)) {
      return windowTitleResultFormat;
    }
    else if (WINDOW_X.equals(key)) {
      return "" + resultsIcon.width; //$NON-NLS-1$
    }
    else if (WINDOW_Y.equals(key)) {
      return "" + resultsIcon.height; //$NON-NLS-1$
    }
    else if (BACKGROUND_COLOR.equals(key)) {
      return ColorConfigurer.colorToString(bgColor);
    }
    else if (TOOLTIP.equals(name)) {
      return tooltip.length() == 0 ? launch.getAttributeValueString(name) : tooltip;
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public Class[] getAllowableConfigureComponents() {
    Class[] c = {SpecialDie.class};
    return c;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("SpecialDiceButton.htm"); //$NON-NLS-1$
  }

  /**
   * create String from int array
   * @param ia int-array
   * @return encoded String
   */
  public static String intArrayToString(int[] ia) {
    if (ia == null || ia.length == 0) {
      return ""; //$NON-NLS-1$
    }
    SequenceEncoder se = new SequenceEncoder(',');
    for (int i = 0; i < ia.length; ++i) {
      se.append(String.valueOf(ia[i]));
    }
    return se.getValue();
  }

  /**
   * get int array from string
   * @param s string with encoded int array
   * @return int array
   */
  public static int[] stringToIntArray(String s) {
    if (s == null
        || s.length() == 0) {
      return EMPTY;
    }
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    ArrayList<String> l = new ArrayList<String>();
    while (st.hasMoreTokens()) {
      l.add(st.nextToken());
    }
    int[] val = new int[l.size()];
    for (int i = 0; i < val.length; ++i) {
      val[i] = Integer.parseInt(l.get(i));
    }
    return val;
  }


  public String encode(Command c) {
    if (c instanceof ShowResults) {
      ShowResults c2 = (ShowResults) c;
      SequenceEncoder se = new SequenceEncoder(c2.target.getIdentifier(), '\t');
      for (int i = 0; i < c2.rolls.length; ++i) {
        se.append(c2.rolls[i] + ""); //$NON-NLS-1$
      }
      return SHOW_RESULTS_COMMAND + se.getValue();
    }
    else {
      return null;
    }
  }

  public Command decode(String s) {
    SequenceEncoder.Decoder st = null;
    if (s.startsWith(SHOW_RESULTS_COMMAND + getConfigureName())
        || s.startsWith(SHOW_RESULTS_COMMAND + getId())) {
      st = new SequenceEncoder.Decoder(s, '\t');
      st.nextToken();
      st.nextToken();
    }
    else if (s.startsWith(getId() + '\t')) { // Backward compatibility
      st = new SequenceEncoder.Decoder(s, '\t');
      st.nextToken();
    }
    if (st != null) {
      ArrayList<String> l = new ArrayList<String>();
      while (st.hasMoreTokens()) {
        l.add(st.nextToken());
      }
      int[] results = new int[l.size()];
      int i = 0;
      for (String n : l) {
        results[i++] = Integer.parseInt(n);
      }
      return new ShowResults(this, results);
    }
    else {
      return null;
    }
  }

  /**
   * Command for displaying the results of a roll of the dice
   */
  public static class ShowResults extends Command {
    private SpecialDiceButton target;
    private int[] rolls;

    public ShowResults(SpecialDiceButton oTarget, int[] results) {
      target = oTarget;
      rolls = new int[results.length];
      System.arraycopy(results, 0, rolls, 0, results.length);
    }

    protected void executeCommand() {
      target.reportResults(rolls);
    }

    protected Command myUndoCommand() {
      return null;
    }
  }

  /** Icon class for graphical display of a dice roll */
  private class ResultsIcon implements Icon {
    private int width, height;
    private Icon[] icons;

    public ResultsIcon() {
    }

    private void setResults(int[] results) {
      icons = new Icon[results.length];
      for (int i = 0; i < results.length; ++i) {
        String imageName = ((SpecialDie) dice.get(i)).getImageName(results[i]);
        try {
          Image aImage = GameModule.getGameModule().getDataArchive().getCachedImage(imageName);
          icons[i] = new ImageIcon(aImage);
        }
        catch (IOException e) {
          System.err.println("Unable to locate image " + imageName); //$NON-NLS-1$
        }
      }
    }

    public void paintIcon(Component c, Graphics g, int x, int y) {
      if (bgColor != null) {
        g.setColor(bgColor);
        g.fillRect(x, y, width, height);
      }
      int offset = 0;
      for (int i = 0; i < icons.length; ++i) {
        if (icons[i] != null) {
          icons[i].paintIcon(c, g, x + offset, y);
          offset += icons[i].getIconWidth();
        }
      }
    }

    public int getIconWidth() {
      return width;
    }

    public int getIconHeight() {
      return height;
    }

  }  // end class SpecialDiceIcon

}
