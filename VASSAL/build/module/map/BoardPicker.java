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
package VASSAL.build.module.map;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.WindowConstants;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.BoardSlot;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.Configurer;
import VASSAL.configure.DoubleConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidityChecker;
import VASSAL.i18n.Resources;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.SequenceEncoder;

/**
 * This class is responsible for maintaining the {@link Board}s on a {@link Map}. As a {@link CommandEncoder}, it
 * recognizes {@link Command}s that specify the set of boards to be used on a map. As a {@link GameComponent} it reacts
 * to the start of a game by prompting the player to select boards if none have been specified
 */
public class BoardPicker extends JDialog implements ActionListener, GameComponent, Configurable, CommandEncoder, ValidityChecker {
  private static final long serialVersionUID = 1L;

  public static final String ID = "BoardPicker"; //$NON-NLS-1$
  protected Vector possibleBoards = new Vector();
  protected Vector currentBoards = null;
  private Dimension psize = new Dimension(350, 125);
  private double slotScale = 0.2;
  protected JTextField status;
  protected JButton cancelButton;
  protected Map map;
  protected JPanel slotPanel;
  protected String version = "0.0"; //$NON-NLS-1$
  protected int nx = 1, ny = 1;
  protected JToolBar controls;
  protected JButton addRowButton;
  protected JButton addColumnButton;
  protected boolean allowMultiple;
  protected int maxColumns;
  protected String title = Resources.getString("BoardPicker.choose_boards"); //$NON-NLS-1$
  protected String addRowButtonText = Resources.getString("BoardPicker.add_row"); //$NON-NLS-1$
  protected String addColumnButtonText = Resources.getString("BoardPicker.add_column"); //$NON-NLS-1$
  protected String boardPrompt = Resources.getString("BoardPicker.select_board"); //$NON-NLS-1$
  protected String defaultSetup;
  protected Vector multipleButtons;
  public static final String SCALE = "slotScale"; //$NON-NLS-1$
  public static final String SLOT_HEIGHT = "slotHeight"; //$NON-NLS-1$
  public static final String SLOT_WIDTH = "slotWidth"; //$NON-NLS-1$
  public static final String SETUP = "setup"; //$NON-NLS-1$
  public static final String DIALOG_TITLE = "title"; //$NON-NLS-1$
  public static final String ADD_ROW_BUTTON_TEXT = "addRowText"; //$NON-NLS-1$
  public static final String ADD_COLUMN_BUTTON_TEXT = "addColumnText"; //$NON-NLS-1$
  public static final String BOARD_PROMPT = "boardPrompt"; //$NON-NLS-1$
  public static final String MAX_COLUMNS = "maxColumns"; //$NON-NLS-1$
  private JButton clearButton;
  private JButton okButton;

  public BoardPicker() {
    super((java.awt.Frame) null, true);
    allowMultiple = false;
  }

  protected void initComponents() {
    multipleButtons = new Vector();
    setTitle(title);
    status = new JTextField(""); //$NON-NLS-1$
    status.setEditable(false);
    slotPanel = new JPanel();
    controls = new JToolBar();
    controls.setFloatable(false);
    controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
    okButton = addButton(Resources.getString(Resources.OK));
    cancelButton = addButton(Resources.getString(Resources.CANCEL));
    addRowButton = addButton(addRowButtonText);
    multipleButtons.addElement(addRowButton);
    addColumnButton = addButton(addColumnButtonText);
    multipleButtons.addElement(addColumnButton);
    clearButton = addButton(Resources.getString("BoardPicker.clear")); //$NON-NLS-1$
    multipleButtons.addElement(clearButton);
    setAllowMultiple(allowMultiple);
    getContentPane().add("North", status); //$NON-NLS-1$
    JPanel pp = new JPanel();
    pp.add(controls);
    getContentPane().add("West", pp); //$NON-NLS-1$
    getContentPane().add("Center", new ScrollPane(slotPanel)); //$NON-NLS-1$
    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent we) {
        cancel();
      }
    });
    reset();
    setLocation(Toolkit.getDefaultToolkit().getScreenSize().width / 2 - getSize().width / 2, Toolkit.getDefaultToolkit().getScreenSize().height / 2
        - getSize().height / 2);
  }

  public Dimension getDefaultSlotSize() {
    return psize;
  }

  /**
   * @return the zoom factory at which to display boards when selecting them
   */
  public double getSlotScale() {
    return slotScale;
  }

  public void warn(String s) {
    if (status != null) {
      status.setText(s);
    }
  }

  public void addTo(Buildable b) {
    map = (Map) b;
    map.setBoardPicker(this);
    for (Enumeration e = possibleBoards.elements(); e.hasMoreElements();) {
      ((Board) e.nextElement()).setMap(map);
    }
  }

  public void build(Element e) {
    if (e == null) {
      Board b = new Board();
      b.build(null);
      b.addTo(this);
    }
    else {
      NodeList l = e.getElementsByTagName(SETUP);
      if (l.getLength() > 0) {
        Element setupEl = (Element) l.item(0);
        defaultSetup = Builder.getText(setupEl);
        Node nextSibling = setupEl.getNextSibling();
        e.removeChild(setupEl);
        Builder.build(e, this);
        e.insertBefore(setupEl, nextSibling);
      }
      else {
        Builder.build(e, this);
      }
      try {
        psize = new Dimension(Integer.parseInt(e.getAttribute(SLOT_WIDTH)), Integer.parseInt(e.getAttribute(SLOT_HEIGHT)));
      }
      catch (Exception ex) {
      }
      try {
        slotScale = Double.valueOf(e.getAttribute(SCALE)).doubleValue();
      }
      catch (Exception ex) {
      }
      try {
        maxColumns = Integer.parseInt(e.getAttribute(MAX_COLUMNS));
      }
      catch (Exception ex) {
        maxColumns = 0;
      }
      String value = e.getAttribute(DIALOG_TITLE);
      if (value != null && value.length() > 0) {
        title = value;
      }
      value = e.getAttribute(ADD_ROW_BUTTON_TEXT);
      if (value != null && value.length() > 0) {
        addRowButtonText = value;
      }
      value = e.getAttribute(ADD_COLUMN_BUTTON_TEXT);
      if (value != null && value.length() > 0) {
        addColumnButtonText = value;
      }
      value = e.getAttribute(BOARD_PROMPT);
      if (value != null && value.length() > 0) {
        boardPrompt = value;
      }
    }
  }

  public void validate(Buildable target, ValidationReport report) {
    if (possibleBoards.size() == 0) {
      report.addWarning(Resources.getString("BoardPicker.must_define", ConfigureTree.getConfigureName(map))); //$NON-NLS-1$
    }
    Set names = new HashSet();
    for (Enumeration e = possibleBoards.elements(); e.hasMoreElements();) {
      Object o = e.nextElement();
      if (o instanceof Board) {
        Board b = (Board) o;
        if (names.contains(b.getName())) {
          report.addWarning(Resources.getString("BoardPicker.more_than_one", b.getName(), ConfigureTree.getConfigureName(map))); //$NON-NLS-1$
        }
        names.add(b.getName());
        b.validate(b, report);
      }
    }
  }

  private String getDefaultSetup() {
    String s = defaultSetup;
    if (defaultSetup == null || defaultSetup.length() == 0) {
      if (possibleBoards.size() == 1 && possibleBoards.firstElement() instanceof Board) {
        Board b = (Board) possibleBoards.firstElement();
        if (!"true".equals(b.getAttributeValueString(Board.REVERSIBLE))) { //$NON-NLS-1$
          Vector v = new Vector();
          v.addElement(b);
          s = encode(new SetBoards(this, v));
        }
      }
    }
    return s;
  }

  /**
   * Add a board to the list of those available for the user to choose from
   */
  public void add(Buildable b) {
    possibleBoards.addElement(b);
  }

  /**
   * Remove a board from the list of those available for the user to choose from
   */
  public void remove(Buildable b) {
    possibleBoards.removeElement(b);
  }

  public void removeFrom(Buildable parent) {
  }

  public static String getConfigureTypeName() {
    return "Map Boards";
  }

  public String getConfigureName() {
    return null;
  }

  public String getBoardDelimiter() {
    return "bd\t"; //$NON-NLS-1$
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Configurer getConfigurer() {
    return new Config();
  }

  public Configurable[] getConfigureComponents() {
    Configurable config[] = new Configurable[possibleBoards.size()];
    for (int i = 0; i < possibleBoards.size(); ++i) {
      config[i] = (Board) possibleBoards.elementAt(i);
    }
    return config;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{Board.class};
  }

  public void addPropertyChangeListener(java.beans.PropertyChangeListener l) {
  }

  public void setBoards(Enumeration bdEnum) {
    reset();
    List l = new ArrayList();
    while (bdEnum.hasMoreElements()) {
      l.add(bdEnum.nextElement());
    }
    for (Iterator e = l.iterator(); e.hasNext();) {
      Board b = (Board) e.next();
      if (b.relativePosition().x > nx - 1)
        addColumn();
      if (b.relativePosition().y > ny - 1)
        addRow();
    }
    for (Iterator e = l.iterator(); e.hasNext();) {
      Board b = (Board) e.next();
      getSlot(b.relativePosition().x + nx * b.relativePosition().y).setBoard(b);
    }
    pack();
  }

  public void pack() {
    super.pack();
    Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
    int maxWidth = d.width - getLocation().x;
    int maxHeight = d.height - getLocation().y;
    setSize(Math.min(getSize().width, maxWidth), Math.min(getSize().height, maxHeight));
  }

  protected void selectBoards() {
    if (currentBoards != null) {
      setBoards(currentBoards.elements());
    }
    else {
      reset();
    }
    cancelButton.setVisible(false);
    setVisible(true);
    cancelButton.setVisible(true);
    if (currentBoards != null && currentBoards.size() > 0) {
      defaultSetup = encode(new SetBoards(this, currentBoards));
    }
    else {
      defaultSetup = null;
    }
  }

  /**
   * @return an Enumeration of boards that have been selected either by the user via the dialog or from reading a
   *         savefile
   */
  public Enumeration getCurrentBoards() {
    return currentBoards == null ? Collections.enumeration(Collections.EMPTY_LIST) : currentBoards.elements();
  }

  /**
   * @return an array of the names of all boards from which the user may choose
   */
  public String[] getAllowableBoardNames() {
    String s[] = new String[possibleBoards.size()];
    for (int i = 0; i < s.length; ++i) {
      s[i] = ((Board) possibleBoards.elementAt(i)).getName();
    }
    return s;
  }

  /**
   * @return a Board with the given name.
   */
  public Board getBoard(String boardName) {
    for (Enumeration e = possibleBoards.elements(); e.hasMoreElements();) {
      Board b = (Board) e.nextElement();
      if (b.getName().equals(boardName)) {
        return b;
      }
    }
    warn(Resources.getString("BoardPicker.board_not_found", boardName)); //$NON-NLS-1$
    return null;
  }

  /**
   * When starting a game, check to see if any boards have been specified (via an encoded {@link Command}. If not, show
   * a dialog to prompt the user for boards. When ending a game, clear the selected boards
   */
  public void setup(boolean show) {
    if (show) {
      if (currentBoards == null) {
        String setup = getDefaultSetup();
        if (setup != null) {
          Command c = decode(setup);
          if (c != null) {
            c.execute();
          }
        }
        if ((currentBoards == null || currentBoards.size() == 0) && possibleBoards.size() > 0) {
          reset();
          setVisible(true);
        }
      }
      map.setBoards(getCurrentBoards());
    }
    else {
      currentBoards = null;
    }
  }

  /**
   * The restore command of a BoardPicker, when executed, sets the boards of its {@link Map} to
   * {@link #getCurrentBoards}
   */
  public Command getRestoreCommand() {
    return new SetBoards(this, currentBoards);
  }

  protected JButton addButton(String s) {
    return addButton(s, -1);
  }

  protected JButton addButton(String s, int index) {
    JButton b = new JButton(s);
    b.addActionListener(this);
    controls.add(b, null, index);
    return b;
  }

  protected void addRow() {
    slotPanel.setLayout(new GridLayout(++ny, nx));
    for (int i = 0; i < nx; ++i) {
      slotPanel.add(new BoardSlot(this, boardPrompt), -1);
    }
    slotPanel.revalidate();
    pack();
  }

  protected void addColumn() {
    slotPanel.setLayout(new GridLayout(ny, ++nx));
    for (int j = 0; j < ny; ++j) {
      slotPanel.add(new BoardSlot(this, boardPrompt), (j + 1) * nx - 1);
    }
    slotPanel.revalidate();
    pack();
  }

  public void actionPerformed(ActionEvent e) {
    if (addColumnButton == e.getSource()) {
      if (maxColumns == 0 || nx < maxColumns) {
        addColumn();
      }
      else {
        addRow();
      }
    }
    else if (addRowButton == e.getSource()) {
      addRow();
    }
    else if (clearButton == e.getSource()) {
      reset();
    }
    else if (okButton == e.getSource()) {
      currentBoards = new Vector(getBoardsFromControls());
      setVisible(false);
    }
    else if (cancelButton == e.getSource()) {
      cancel();
    }
  }

  protected void cancel() {
    GameModule.getGameModule().getGameState().setup(false);
    setVisible(false);
  }

  /**
   * @deprecated use {@link #getBoardsFromControls()}
   * @return
   */
  public Vector pickBoards() {
    return new Vector(getBoardsFromControls());
  }

  /**
   * Return the list of boards as specified in the current controls
   * 
   * @return
   */
  public List getBoardsFromControls() {
    List boardList = new ArrayList();
    if (controls != null) {
      // Adjust the bounds of each board according to its relative position
      for (int i = 0; i < nx; ++i) {
        for (int j = 0; j < ny; ++j) {
          BoardSlot slot = getSlot(i + nx * j);
          if (slot != null) {
            Board b = slot.getBoard();
            if (b != null) {
              b.relativePosition().move(i, j);
              boardList.add(b);
            }
          }
        }
      }
    }
    return boardList;
  }

  public void reset() {
    if (controls == null) {
      initComponents();
    }
    else {
      warn(""); //$NON-NLS-1$
      removeAllBoards();
      slotPanel.add(new BoardSlot(this, boardPrompt), 0);
      pack();
    }
  }

  public int getRowCount() {
    return ny;
  }

  public int getColumnCount() {
    return ny;
  }

  public BoardSlot getNeighbor(BoardSlot slot, int dx, int dy) {
    int x = -1, y = -1;
    for (int i = 0; i < nx; ++i) {
      for (int j = 0; j < ny; ++j) {
        if (getSlot(i + j * nx) == slot) {
          x = i;
          y = j;
          break;
        }
        if (x >= 0 && y >= 0) {
          break;
        }
      }
    }
    x += dx;
    y += dy;
    if (x < 0 || x >= nx || y < 0 || y >= ny) {
      return null;
    }
    int index = x + y * nx;
    if (index < 0 || index >= nx * ny) {
      return null;
    }
    return getSlot(index);
  }

  public BoardSlot getSlot(int i) {
    return i >= 0 && i < slotPanel.getComponentCount() ? (BoardSlot) slotPanel.getComponent(i) : null;
  }

  public void repaintAll() {
    for (int i = 0; i < nx; ++i) {
      for (int j = 0; j < ny; ++j) {
        getSlot(i + nx * j).repaint();
      }
    }
  }

  protected void removeAllBoards() {
    // for(int n=slotPanel.getComponentCount()-1;n>=0;--n)
    // slotPanel.remove(slotPanel.getComponent(n));
    slotPanel.removeAll();
    slotPanel.setLayout(new GridLayout(1, 1));
    nx = ny = 1;
  }

  /**
   * @return true if multiple boards per map window are allowed
   */
  public boolean isAllowMultiple() {
    return allowMultiple;
  }

  public void setAllowMultiple(boolean val) {
    allowMultiple = val;
    if (multipleButtons != null) {
      for (Enumeration e = multipleButtons.elements(); e.hasMoreElements();) {
        ((JButton) e.nextElement()).setVisible(allowMultiple);
      }
    }
  }

  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    org.w3c.dom.Element el = doc.createElement(getClass().getName());
    el.setAttribute(SLOT_WIDTH, String.valueOf(psize.width));
    el.setAttribute(SLOT_HEIGHT, String.valueOf(psize.height));
    el.setAttribute(SCALE, String.valueOf(getSlotScale()));
    el.setAttribute(DIALOG_TITLE, title);
    el.setAttribute(ADD_ROW_BUTTON_TEXT, addRowButtonText);
    el.setAttribute(ADD_COLUMN_BUTTON_TEXT, addColumnButtonText);
    el.setAttribute(BOARD_PROMPT, boardPrompt);
    if (maxColumns > 0) {
      el.setAttribute(MAX_COLUMNS, String.valueOf(maxColumns));
    }
    if (defaultSetup != null) {
      Element setupEl = doc.createElement(SETUP);
      setupEl.appendChild(doc.createTextNode(defaultSetup));
      el.appendChild(setupEl);
    }
    for (Enumeration e = possibleBoards.elements(); e.hasMoreElements();) {
      Board b = (Board) e.nextElement();
      el.appendChild(b.getBuildElement(doc));
    }
    return el;
  }

  public Command decode(String command) {
    if (command.startsWith(map.getId() + ID) || command.startsWith(map.getConfigureName() + ID)) {
      Vector bds = new Vector();
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, '\t');
      st.nextToken();
      while (st.hasMoreTokens()) {
        SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(st.nextToken(), '/');
        String name = st2.nextToken();
        boolean reversed = false;
        if (st2.hasMoreTokens()) {
          reversed = "rev".equals(st2.nextToken()); //$NON-NLS-1$
        }
        Point p = new Point(st.nextInt(0), st.nextInt(0));
        Board b = getBoard(name);
        if (b != null) {
          if (bds.contains(b)) {
            b = b.copy();
          }
          b.setReversed(reversed);
          b.relativePosition().move(p.x, p.y);
          bds.addElement(b);
        }
      }
      return new SetBoards(this, bds);
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    if (c instanceof SetBoards && map != null && ((SetBoards) c).target == this) {
      SequenceEncoder se = new SequenceEncoder(map.getIdentifier() + ID, '\t');
      Vector bds = ((SetBoards) c).bds;
      if (bds != null) {
        for (Enumeration e = bds.elements(); e.hasMoreElements();) {
          Board b = (Board) e.nextElement();
          SequenceEncoder se2 = new SequenceEncoder(b.getName(), '/');
          if (b.isReversed()) {
            se2.append("rev"); //$NON-NLS-1$
          }
          se.append(se2.getValue());
          se.append("" + b.relativePosition().x).append("" + b.relativePosition().y); //$NON-NLS-1$ //$NON-NLS-2$
        }
      }
      return se.getValue();
    }
    else {
      return null;
    }
  }
  public static class SetBoards extends Command {
    private Vector bds;
    private BoardPicker target;

    public SetBoards(BoardPicker target, Vector boards) {
      this.target = target;
      bds = boards;
    }

    protected void executeCommand() {
      target.currentBoards = bds;
      if (GameModule.getGameModule().getGameState().isGameStarted()) {
        target.map.setBoards(target.getCurrentBoards());
        target.map.getView().revalidate();
      }
    }

    protected Command myUndoCommand() {
      return null;
    }
  }
  private class Config extends Configurer {
    private JPanel controls;
    private JButton selectButton;
    private IntConfigurer width;
    private IntConfigurer height;
    private DoubleConfigurer scale;
    private StringConfigurer title;
    private StringConfigurer prompt;

    public Config() {
      super(null, null);
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
      title = new StringConfigurer(null, "Dialog Title:  ", BoardPicker.this.title);
      title.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          if (evt.getNewValue() != null) {
            BoardPicker.this.title = (String) evt.getNewValue();
            if (controls != null) {
              setTitle(BoardPicker.this.title);
            }
          }
        }
      });
      controls.add(title.getControls());
      prompt = new StringConfigurer(null, "\"Select Boards\" prompt:  ", BoardPicker.this.boardPrompt);
      prompt.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          if (evt.getNewValue() != null) {
            BoardPicker.this.boardPrompt = (String) evt.getNewValue();
          }
        }
      });
      controls.add(prompt.getControls());
      scale = new DoubleConfigurer(null, "Cell scale factor:  ", new Double(slotScale));
      scale.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          if (evt.getNewValue() != null) {
            slotScale = ((Double) evt.getNewValue()).doubleValue();
          }
        }
      });
      controls.add(scale.getControls());
      width = new IntConfigurer(null, "Cell width:  ", new Integer(psize.width));
      width.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          if (evt.getNewValue() != null) {
            psize.width = ((Integer) evt.getNewValue()).intValue();
          }
        }
      });
      controls.add(width.getControls());
      height = new IntConfigurer(null, "Cell height:  ", new Integer(psize.height));
      height.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          if (evt.getNewValue() != null) {
            psize.height = ((Integer) evt.getNewValue()).intValue();
          }
        }
      });
      controls.add(height.getControls());
      selectButton = new JButton(Resources.getString("BoardPicker.select_default")); //$NON-NLS-1$
      selectButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          selectBoards();
        }
      });
      controls.add(selectButton);
    }

    public Component getControls() {
      return controls;
    }

    public String getValueString() {
      return null;
    }

    public void setValue(String s) {
    }
  }
  public String getI18nKey() {
    return "BoardPicker";
  }
}
