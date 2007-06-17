/*
 * $Id: TurnTracker.java 958 2006-08-09 13:14:54 +0000 (Wed, 09 Aug 2006) swampwallaby $
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package VASSAL.build.module.turn;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Iterator;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.border.BevelBorder;
import javax.swing.border.Border;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;

/**
 * Generic Turn Counter
 */
public class TurnTracker extends TurnComponent implements CommandEncoder, GameComponent, ActionListener, UniqueIdManager.Identifyable {

  protected static UniqueIdManager idMgr = new UniqueIdManager("TurnTracker");
  protected PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);
  
  protected static final String COMMAND_PREFIX = "TURN";
  public static final String VERSION = "1.8";

  public static final String NAME = "name";
  public static final String HOT_KEY = "hotkey";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";
  public static final String TURN_FORMAT = "turnFormat";
  public static final String REPORT_FORMAT = "reportFormat";
  public static final String COLOR = "color";
  public static final String TOOLTIP = "tooltip";
  public static final String LENGTH = "length";
  
  private static final String FONT_SIZE = "size";
  private static final String FONT_STYLE = "style";

  /** Variable name for reporting format */
  public static final String OLD_TURN = "oldTurn";
  public static final String NEW_TURN = "newTurn";
  public static final String LEVEL = "level";
  
  public static final String TURN_FONT = "Dialog";
  public static final String SET_COMMAND = "Set Turn";
  public static final String PLAIN_COMMAND = "Plain";
  public static final String BOLD_COMMAND = "Bold";
  
  public static final String NEXT = "Next";
  public static final String PREV = "Prev";
  public static final String SET = "Set";
  
  public static final String PROP_VALUE = "_value";
  public static final String PROP_NAME = "_name";
  public static final String PROP_COMMAND = "_command";
  
  public static final String[] FONT_FAMILYS = new String[] { "Dialog", "DialogInput", "Monospaced", "SanSerif", "Serif"};

  protected FormattedString turnFormat = new FormattedString("$"+LEVEL+"1$ $"+LEVEL+"2$ $"+LEVEL+"3$ $"+LEVEL+"4$");

  protected FormattedString reportFormat = new FormattedString("* <$" + GlobalOptions.PLAYER_ID
      + "$> Turn Updated from $"+OLD_TURN+"$ to $"+NEW_TURN+"$");
  
  protected Color color = Color.white;

  protected TurnWindow turnWindow;
  protected SetDialog setDialog;
  protected LaunchButton launch;
  protected JTextArea turnLabel = new JTextArea();
  protected String tooltip = "Right-click to configure";
  protected int length = 0;
  
  protected String savedState = "";
  protected String savedSetState = "";
  protected String savedTurn = "";
  protected JPopupMenu popup;
  
  protected int currentLevel = 0;
  protected String id;
  
  protected MutableProperty.Impl lastCommand = new MutableProperty.Impl(SET,this);

  public TurnTracker() {
    
    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        turnWindow.setControls();
        turnWindow.setVisible(!turnWindow.isShowing());
      }
    };
    launch = new LaunchButton("Turn", BUTTON_TEXT, HOT_KEY, ICON, al);
    launch.setToolTipText("Turn Tracker");
    launch.setEnabled(false);
    
  }
  
  public String getState() {
    SequenceEncoder se = new SequenceEncoder('|');
    se.append(currentLevel);
    Iterator i = getTurnLevels();
    while (i.hasNext()) {
      TurnLevel level = (TurnLevel) i.next();
      se.append(level.getState());
    }
    return se.getValue();
  }

  public void setState(String newState) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(newState, '|');
    currentLevel = sd.nextInt(0);
    Iterator i = getTurnLevels();
    while (i.hasNext()) {
      TurnLevel level = (TurnLevel) i.next();
      level.setState(sd.nextToken(""));
    }
    
    setLaunchToolTip();
    updateTurnDisplay(SET);
  }
  
  protected void setLaunchToolTip() {
    launch.setToolTipText(getTurnString());
  }
  
  /*
   * Module level Configuration stuff
   */
  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON, HOT_KEY, COLOR, TURN_FORMAT, REPORT_FORMAT, TOOLTIP, LENGTH };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      clearGlobalProperties();
      setConfigureName((String) value);
      lastCommand.setPropertyName(getConfigureName()+PROP_COMMAND);
    }
    else if (REPORT_FORMAT.equals(key)) {
      reportFormat.setFormat((String) value);
    }
    else if (TURN_FORMAT.equals(key)) {
      turnFormat.setFormat((String) value);
    }
    else if (TOOLTIP.equals(key)) {
      tooltip = ((String) value);
      turnLabel.setToolTipText(tooltip);
    }
    else if (LENGTH.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      length = ((Integer) value).intValue();
      turnLabel.setColumns(length);
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      color = (Color) value;
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  protected void setDisplayFont() {
    turnLabel.setFont(getDisplayFont());
    if (turnWindow != null) {
      turnWindow.pack();
    }
  }
  
  protected Font getDisplayFont() {
    int style = getFontStyle();
    int size = getFontSize();
    return new Font(TURN_FONT, style, size);
  }
  
  protected void setFontStyle(int style) {
    GameModule.getGameModule().getPrefs().setValue(FONT_STYLE, new Integer(style));
    setDisplayFont();
  }
  
  protected int getFontStyle() {
    return ((Integer) GameModule.getGameModule().getPrefs().getValue(FONT_STYLE)).intValue();
  }
  
  protected void setFontSize(int size) {
    GameModule.getGameModule().getPrefs().setValue(FONT_SIZE, new Integer(size));
    setDisplayFont();
  }
  
  protected int getFontSize() {
    return ((Integer) GameModule.getGameModule().getPrefs().getValue(FONT_SIZE)).intValue();
  }
  
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName() + "";
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat.getFormat();
    }
    else if (TURN_FORMAT.equals(key)) {
      return turnFormat.getFormat();
    }
    else if (TOOLTIP.equals(key)) {
      return tooltip;
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    else if (LENGTH.equals(key)) {
      return String.valueOf(length);
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Button text:  ", "Button Icon:  ", "Hotkey:  ", "Background Color:  ",
        "Turn Format", "Report Format:  ", "Display Tooltip Text:  ", "Display length of Turn label (0 for variable):  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, IconConfig.class, KeyStroke.class, Color.class, 
        TurnFormatConfig.class, ReportFormatConfig.class, String.class, Integer.class };
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((TurnTracker) c).launch.getAttributeValueString(ICON));
    }
  }

  public static class TurnFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      TurnTracker t = (TurnTracker) c;
      String s[] = new String[t.getLevelCount()];
      for (int i = 0; i < s.length; i++) {
        s[i] = LEVEL+(i+1);
      }
      return new FormattedStringConfigurer(key, name, s);
    }
  }
  
  public static class ReportFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] {OLD_TURN, NEW_TURN } );
    }
  }

  
  public Class[] getAllowableConfigureComponents() {
    return new Class[] { CounterTurnLevel.class, ListTurnLevel.class, TurnGlobalKeyCommand.class };
  }

  public static String getConfigureTypeName() {
    return "Turn Tracker v"+VERSION;
  }

  public void addTo(Buildable b) {
    GameModule.getGameModule().getToolBar().add(launch);
    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    idMgr.add(this);
    
    // Create preferences for Turn text
    final IntConfigurer size = new IntConfigurer(FONT_SIZE, "", new Integer(14));
    final IntConfigurer style = new IntConfigurer(FONT_STYLE, "", new Integer(Font.BOLD));
    
    GameModule.getGameModule().getPrefs().addOption(null, size);
    GameModule.getGameModule().getPrefs().addOption(null, style);
 
    //Global Property support
//    propertyChangeSupport.addPropertyChangeListener(((MutablePropertiesContainer) b).getPropertyListener());
    lastCommand.addTo((MutablePropertiesContainer) b);
    
    //Create the turn window
    turnWindow = new TurnWindow();    
    turnWindow.setColor(color);
    turnWindow.pack(); 

  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(launch);
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    lastCommand.removeFromContainer();
    clearGlobalProperties();
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }
  
  protected void captureState() {
    savedState = getState();
    savedTurn = getTurnString();
  }

  protected void save() {

    if (!savedState.equals(getState())) {
      
      reportFormat.setProperty(OLD_TURN, savedTurn);
      reportFormat.setProperty(NEW_TURN, getTurnString());

      String s = updateString(reportFormat.getText(), new String[] { "\\n", "\\t" }, new String[] { " - ", " " });
      Command c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), s);
      c.execute();
      c.append(new SetTurn(this, savedState));

      GameModule.getGameModule().sendAndLog(c);
      
      setLaunchToolTip();
    }
    
    captureState();

  }
  
  protected String getTurnString() {
    return turnFormat.getText(GameModule.getGameModule());
//    turnFormat.clearProperties();
//    ArrayList turnDesc = getLevelStrings();
//    for (int i = 0; i < turnDesc.size(); i++) {
//      turnFormat.setProperty(LEVEL+(i+1), (String) turnDesc.get(i));
//    }
//    for (int i = turnDesc.size(); i < 10; i++) {
//      turnFormat.setProperty(LEVEL+(i+1), null);
//    }
//    return turnFormat.getText();
  }
  
  protected ArrayList getLevelStrings() {
    ArrayList turnDesc = new ArrayList(5);
    TurnLevel level = getTurnLevel(currentLevel);
    if (level != null) {
      level.getTurnStrings(turnDesc);
    }
    return turnDesc;
  }
  
  protected ArrayList getLevelValues() {
    ArrayList turnDesc = new ArrayList(5);
    TurnLevel level = getTurnLevel(currentLevel);
    if (level != null) {
      level.getTurnValues(turnDesc);
    }
    return turnDesc;
  }
  
  protected ArrayList getLevelNames() {
    ArrayList turnDesc = new ArrayList(5);
    TurnLevel level = getTurnLevel(currentLevel);
    if (level != null) {
      level.getTurnNames(turnDesc);
    }
    return turnDesc;
  }
  
  protected int getLevelCount() {
    return getLevelStrings().size();
  }

  protected void next() {

    if (getTurnLevelCount() == 0) {
      return;
    }

    TurnLevel level = getTurnLevel(currentLevel);
    level.advance();
    if (level.hasRolledOver()) {
      currentLevel++;
      if (currentLevel >= getTurnLevelCount()) {
        currentLevel = 0;
      }
      getTurnLevel(currentLevel).setLow();
    }
    
    updateTurnDisplay(NEXT);
    doGlobalkeys();
  }
  
  protected void prev() {

    if (getTurnLevelCount() == 0) {
      return;
    }

    TurnLevel level = getTurnLevel(currentLevel);
    level.retreat();
    if (level.hasRolledOver()) {
      currentLevel--;
      if (currentLevel < 0) {
        currentLevel = getTurnLevelCount()-1;
      }
      getTurnLevel(currentLevel).setHigh();
    }

    updateTurnDisplay(PREV);
    doGlobalkeys();
  }

  protected void doGlobalkeys() {
    for (TurnGlobalKeyCommand key : getComponentsOf(TurnGlobalKeyCommand.class)) {
      key.apply();
    }
  }

  public void actionPerformed(ActionEvent e) {
    if (e.getActionCommand().equals(SET_COMMAND)) {
      set();
    }
    else if (e.getActionCommand().equals(PLAIN_COMMAND)) {
      setFontStyle(Font.PLAIN);
    }
    else if (e.getActionCommand().equals(BOLD_COMMAND)) {
      setFontStyle(Font.BOLD);
    }
    else {
      try {
        int size = Integer.parseInt(e.getActionCommand());
        setFontSize(size);
      }
      catch (Exception ex) {
        
      }
    }
  }
  
  protected void set() {
    savedSetState = getState();
    if (setDialog == null) {
      setDialog = new SetDialog();
      setDialog.setTitle("Set " + getConfigureName());
    }
    setDialog.setControls(this);
    //setSetVisibility(false);
    setDialog.setVisible(true);
  }

  protected void updateTurnDisplay(String command) {
    this.lastCommand.setPropertyValue(command);
    turnWindow.setControls();
    turnWindow.setColor(color);
    turnWindow.repaint();
  }

  protected void clearGlobalProperties() {
    lastCommand.setPropertyValue(null);
  }
  
  public Command decode(String command) {
    Command comm = null;
    if (command.startsWith(COMMAND_PREFIX+getId())) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(command, '\t');
      sd.nextToken("");
     comm = new SetTurn(sd.nextToken(""), this);
    }
    return comm;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof SetTurn) {
      SetTurn com = (SetTurn) c;
      SequenceEncoder se = new SequenceEncoder('\t');
      se.append(COMMAND_PREFIX + com.getTurn().getId());
      se.append(com.newState);
      return se.getValue();
    }
    return s;
  }
  
  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting);
    if (gameStarting) {
      lastCommand.setPropertyValue(SET);
    }
    else {
      turnWindow.setVisible(false);
      reset();
    }
  }

  protected void reset() {
    for (int i = 0; i < getTurnLevelCount(); i++) {
      (getTurnLevel(i)).reset();
    }
    currentLevel = 0;
    setLaunchToolTip();
    clearGlobalProperties();
  }
  
  public String updateString(String str, String[] from, String[] to) {
    
    StringBuffer s = new StringBuffer(str);
  
    for (int i = 0; i < from.length; i++) {
      replace(s, from[i], to[i]);
    }
    
    return s.toString();
  }
  
  public void replace(StringBuffer s, String from, String to) {
    
    int i = s.indexOf(from);
    while (i >= 0) {
      s = s.replace(i, i+2, to);
      i = s.indexOf(from);
    }
  }
  
  public Command getRestoreCommand() {
    return new SetTurn(getState(), this);
  }

  protected class TurnWindow extends JDialog implements MouseListener {

  private static final long serialVersionUID = 1L;
  
  protected final int BUTTON_SIZE = 25;
    protected JPanel mainPanel;
    protected JPanel controlPanel;
    protected JPanel turnPanel;
    protected JPanel leftPanel;
    protected JPanel rightPanel;

    protected TurnWindow() {
      super(GameModule.getGameModule().getFrame());
      initComponents();
    }


    public void setColor(Color color) {
      turnPanel.setBackground(color);
      turnLabel.setBackground(color);    
    }

    protected void initComponents() {

      setTitle(getConfigureName());

      mainPanel = new JPanel();
      mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
      getContentPane().add(mainPanel);

      // Create a panel to contain the next/prev buttons
      controlPanel = new JPanel();
      controlPanel.setLayout(new BoxLayout(controlPanel, BoxLayout.X_AXIS));
      
      leftPanel = new JPanel();
      //leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
      //leftPanel.setPreferredSize(new Dimension(BUTTON_SIZE, BUTTON_SIZE*2));
      
      JLabel nextButton = new IconButton(PLUS_ICON, BUTTON_SIZE, BUTTON_SIZE);
      nextButton.setToolTipText("Next Turn");
      nextButton.setAlignmentY(Component.TOP_ALIGNMENT);
      leftPanel.add(nextButton);
      nextButton.addMouseListener(new MouseAdapter() {
        public void mouseClicked(MouseEvent arg0) {
          captureState();
          next();
          save();
        }
        });

      rightPanel = new JPanel();
      //rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
      //rightPanel.setPreferredSize(new Dimension(BUTTON_SIZE, BUTTON_SIZE*2));
      
      JLabel prevButton = new IconButton(MINUS_ICON, BUTTON_SIZE, BUTTON_SIZE);
      prevButton.setToolTipText("Previous Turn");
      prevButton.setAlignmentY(Component.TOP_ALIGNMENT);
      rightPanel.add(prevButton);
      prevButton.addMouseListener(new MouseAdapter() {
        public void mouseClicked(MouseEvent arg0) {
          captureState();
          prev();
          save();
        }
        });
      
 //     leftPanel.add(Box.createVerticalGlue());
 
      
      // Next, the Label containing the Turn Text
      setDisplayFont();
      turnLabel.setEditable(false);
      turnLabel.setFocusable(false);
      
      turnPanel = new JPanel();
      turnPanel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
      //turnPanel.setLayout(new BoxLayout(turnPanel, BoxLayout.X_AXIS));
      turnPanel.add(BorderLayout.CENTER, turnLabel);
      turnLabel.addMouseListener(this);
 
      controlPanel.add(rightPanel);
      controlPanel.add(turnPanel);
      controlPanel.add(leftPanel);
      
      mainPanel.add(controlPanel);
      
      addMouseListener(this);

      pack();
      setLocation(100, 100);
    }

    public void setControls() {
      String s = updateString(getTurnString(), new String[] { "\\n", "\\t" }, new String[] { "\n", "    " });
      
      turnLabel.setText(s);
      pack();
      
//      TextLayout layout = new TextLayout(getLongestTurn(), turnLabel.getFont(), new FontRenderContext(new AffineTransform(), true, false));
//      turnLabel.setPreferredSize(new Dimension((int) layout.getBounds().getWidth()+10, (int) layout.getBounds().getHeight()+10));
//      turnLabel.repaint();
    }
           
    public void mouseClicked(MouseEvent e) {
      if (e.isMetaDown()) {
        doPopup(e.getPoint());
      }
    }

    public void mouseEntered(MouseEvent e) {
      
    }

    public void mouseExited(MouseEvent e) {
      
    }

    public void mousePressed(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
    }
    
    public void doPopup(Point p) {
      if (popup == null) {
        buildPopup();
      }

      popup.show(this, p.x, p.y);
    }
    
  }
  
  protected void buildPopup() {
    
    popup = new JPopupMenu();
    popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
      public void popupMenuCanceled(javax.swing.event.PopupMenuEvent evt) {
        turnWindow.repaint();
      }

      public void popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent evt) {
        turnWindow.repaint();
      }

      public void popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent evt) {
      }
    });
    
    JMenuItem item = new JMenuItem(SET_COMMAND);
    item.addActionListener(this);
    popup.add(item);
    
    // Configure List Items
    JMenu config = new JMenu("Configure");
    
    for (int i = 0; i < getTurnLevelCount(); i++) {
      getTurnLevel(i).buildConfigMenu(config);
    }
    
    if (config.getItemCount() > 0) {
      popup.add(config);
    }
    
    // Configure Font
    JMenu font = new JMenu("Font");
    
    JMenu size = new JMenu("Size");
    addItem(size, "10");
    addItem(size, "12");
    addItem(size, "14");
    addItem(size, "16");
    addItem(size, "18");
    addItem(size, "20");    
    font.add(size);
    
    JMenu style = new JMenu("Style");
    addItem(style, PLAIN_COMMAND);
    addItem(style, BOLD_COMMAND);
    font.add(style);
    
    popup.add(font);
  }

  protected void addItem(JMenu menu, String command) {
    JMenuItem item = new JMenuItem(command);
    item.addActionListener(this);
    menu.add(item);
  }
  
  public static final int PLUS_ICON = 0;
  public static final int MINUS_ICON = 1;
  public static final int TICK_ICON = 2;
  public static final int CROSS_ICON = 3;
  
  protected class IconButton extends JLabel implements MouseListener {

    private static final long serialVersionUID = 1L;
    
    protected int type;
    protected int w, h;
    Border raised = BorderFactory.createBevelBorder(BevelBorder.RAISED);
    Border lowered = BorderFactory.createBevelBorder(BevelBorder.LOWERED);
    
    
    public IconButton(int t, int w, int h) {
      super();
      this.type = t;
      this.w = w;
      this.h = h;
      //setMaximumSize(new Dimension(w, h));
      setMinimumSize(new Dimension(w, h));
      setPreferredSize(new Dimension(w, h));
      setBorder(raised);
      addMouseListener(this);
    }
    
    public void paint(Graphics g) {
      super.paint(g);
      Graphics2D g2 = (Graphics2D) g;
      g2.setStroke(new BasicStroke(2f));
      //g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      Rectangle r = getBounds();
      
      switch (type) {
        case PLUS_ICON:
          g.drawLine(5, r.height/2, r.width-5, r.height/2);
          g.drawLine(r.width/2, 5, r.width/2, r.height-5);
          break;
        case MINUS_ICON:
          g.drawLine(5, r.height/2, r.width-5, r.height/2);
          break;
        case TICK_ICON:
          break;
        case CROSS_ICON:
          break;        
      }
    }


    public void mouseClicked(MouseEvent arg0) {
      
    }

    public void mouseEntered(MouseEvent arg0) {
      
    }

    public void mouseExited(MouseEvent arg0) {
      
    }

    public void mousePressed(MouseEvent arg0) {
      setBorder(lowered);
      repaint();
    }

    public void mouseReleased(MouseEvent arg0) {
      setBorder(raised);
      repaint();
    }
  }
  
  private static final Dimension FILLER = new Dimension(0, 3);
  
  protected class SetDialog extends JDialog {

    private static final long serialVersionUID = 1L;
    
    protected JPanel panel;
    protected JPanel controls = null;
    protected ArrayList levelPanels = new ArrayList();
    protected JPanel levelControls = null;
    protected Component childControls = null;
    protected TurnTracker turn;
    protected JDialog me;

    protected SetDialog() {
      super(GameModule.getGameModule().getFrame());
      initComponents();
      setLocation(100, 100);
      me = this;
    }
    
    protected void initComponents() {
      getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          cancelSet();
          setVisible(false);
        }
      });

      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      getContentPane().add(panel);

      JPanel p = new JPanel();

      JButton saveButton = new JButton("Save");
      saveButton.setToolTipText("Save Changes to Turn Counter");
      p.add(saveButton);
      saveButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          saveSet();
          setVisible(false);
        }
      });

      JButton cancelButton = new JButton("Cancel");
      cancelButton.setToolTipText("Discard Changes to Turn Counter");
      cancelButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          cancelSet();
          setVisible(false);
        }
      });
      p.add(cancelButton);
      
      getContentPane().add(p);
    }

    public void setControls(TurnTracker turn) {
      
      this.turn = turn;
      
      if (controls != null) {
        panel.remove(controls);
      }

      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      levelControls = new JPanel();
      levelControls.setLayout(new BoxLayout(levelControls, BoxLayout.Y_AXIS));
      
      if (getTurnLevelCount() > 1) {
        JPanel p = new JPanel();
        p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
        p.setBorder(BorderFactory.createLineBorder(Color.black));
        
        String s[] = new String[getTurnLevelCount()];
        for (int i = 0; i < s.length; i++) {
          s[i] = getTurnLevel(i).getConfigureName();
        }    
        StringEnumConfigurer e = new StringEnumConfigurer(null, " Select:  ", s);
        e.setValue(getTurnLevel(currentLevel).getConfigureName());
        e.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent e) {
            String option = ((StringEnumConfigurer) e.getSource()).getValueString();
            for (int i = 0; i < getTurnLevelCount(); i++) {
              if (option.equals(getTurnLevel(i).getConfigureName())) {
                  currentLevel = i;
                  updateTurnDisplay(SET);
                  addChildControls();
              }
            }
          }});
        
        p.add(Box.createRigidArea(FILLER));
        p.add(e.getControls());
        p.add(Box.createRigidArea(FILLER));
        levelControls.add(p);
        levelControls.add(Box.createRigidArea(FILLER));
        
      }
      
      addChildControls();
      
      controls.add(levelControls);

      panel.add(controls);
      pack();
    }
    
    protected void addChildControls () {
      if (childControls != null) {
        levelControls.remove(childControls);
      }
      childControls = getTurnLevel(currentLevel).getSetControls(me, turn);
      levelControls.add(childControls);
      pack();
    }
     
  }
  
  protected void cancelSet() {
    setState(savedSetState);
    turnWindow.setVisible(true);
  }
  
  protected void saveSet() {
    save();
    updateTurnDisplay(SET);
    doGlobalkeys();
  }

  public static class SetTurn extends Command {
    private String oldState;
    private String newState;
    private TurnTracker turn;

    public SetTurn(String newState, TurnTracker t) {
      this.newState = newState;
      oldState = t.getState();
      turn = t;
    }
    
    public SetTurn(TurnTracker t, String oldState) {
      newState = t.getState();
      this.oldState = oldState;
      turn = t;
    }

    public TurnTracker getTurn() {
      return turn;
    }
    
    protected void executeCommand() {
      turn.setState(newState);
    }

    protected Command myUndoCommand() {
      return new SetTurn(oldState, turn);
    }
  }
}
