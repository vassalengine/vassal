/*
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

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.GameSetupStep;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.BoardSlot;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.ComponentConfigPanel;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.Configurer;
import VASSAL.configure.DoubleConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidityChecker;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.i18n.Translatable;
import VASSAL.tools.ProblemDialog;
import VASSAL.tools.SequenceEncoder;

import VASSAL.tools.swing.SwingUtils;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Vector;
import java.util.stream.Collectors;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.WindowConstants;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * This class is responsible for maintaining the {@link Board}s on a {@link Map}. As a {@link CommandEncoder}, it
 * recognizes {@link Command}s that specify the set of boards to be used on a map. As a {@link GameComponent} it reacts
 * to the start of a game by prompting the player to select boards if none have been specified.
 */
public class BoardPicker extends AbstractBuildable implements ActionListener, GameComponent, GameSetupStep, Configurable, CommandEncoder, ValidityChecker {
  public static final String ID = "BoardPicker"; //$NON-NLS-1$
  protected List<Board> possibleBoards = new ArrayList<>();
  protected List<Board> currentBoards = null;
  protected Dimension psize = new Dimension(350, 125);
  protected double slotScale = 0.2;
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected JTextField status;
  protected JLabel statusLabel;
  protected Map map;
  protected JPanel slotPanel;
  protected String version = "0.0"; //$NON-NLS-1$
  protected int nx = 1, ny = 1;
  protected JToolBar toolbar;
  protected JPanel controls;
  protected JButton addRowButton;
  protected JButton addColumnButton;
  protected boolean allowMultiple;
  protected int maxColumns;
  protected String title = Resources.getString("BoardPicker.choose_boards"); //$NON-NLS-1$
  protected String addRowButtonText = Resources.getString("BoardPicker.add_row"); //$NON-NLS-1$
  protected String addColumnButtonText = Resources.getString("BoardPicker.add_column"); //$NON-NLS-1$
  protected String boardPrompt = Resources.getString("BoardPicker.select_board"); //$NON-NLS-1$
  protected String defaultSetup;
  protected List<JButton> multipleButtons;
  public static final String SCALE = "slotScale"; //$NON-NLS-1$
  public static final String SLOT_HEIGHT = "slotHeight"; //$NON-NLS-1$
  public static final String SLOT_WIDTH = "slotWidth"; //$NON-NLS-1$
  public static final String SETUP = "setup"; //$NON-NLS-1$
  public static final String DIALOG_TITLE = "title"; //$NON-NLS-1$
  public static final String ADD_ROW_BUTTON_TEXT = "addRowText"; //$NON-NLS-1$
  public static final String ADD_COLUMN_BUTTON_TEXT = "addColumnText"; //$NON-NLS-1$
  public static final String BOARD_PROMPT = "boardPrompt"; //$NON-NLS-1$
  public static final String MAX_COLUMNS = "maxColumns"; //$NON-NLS-1$
  protected JButton clearButton;
  protected JButton okButton;
  protected ComponentI18nData myI18nData;
  protected JScrollPane slotScroll;

  public BoardPicker() {
    allowMultiple = false;
  }

  protected void initComponents() {
    multipleButtons = new ArrayList<>();
    controls = new JPanel(new BorderLayout());
    statusLabel = new JLabel(""); //$NON-NLS-1$
    statusLabel.setForeground(Color.BLUE);
    slotPanel = new JPanel();
    toolbar = new JToolBar();
    toolbar.setFloatable(false);
    toolbar.setLayout(new BoxLayout(toolbar, BoxLayout.Y_AXIS));
    addRowButton = addButton(addRowButtonText);
    multipleButtons.add(addRowButton);
    addColumnButton = addButton(addColumnButtonText);
    multipleButtons.add(addColumnButton);
    clearButton = addButton(Resources.getString("BoardPicker.clear")); //$NON-NLS-1$
    multipleButtons.add(clearButton);
    setAllowMultiple(allowMultiple);
    controls.add(BorderLayout.NORTH, statusLabel);
    final JPanel pp = new JPanel();
    pp.add(toolbar);
    controls.add(BorderLayout.WEST, pp);
    slotScroll = new JScrollPane(slotPanel);
    controls.add(BorderLayout.CENTER, slotScroll);
    reset();
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
    if (statusLabel != null) {
      statusLabel.setText(s);
    }
  }

  @Override
  public void addTo(Buildable b) {
    map = (Map) b;
    map.setBoardPicker(this);
    for (final Board board : possibleBoards) {
      board.setMap(map);
    }
    if (b instanceof Translatable) {
      getI18nData().setOwningComponent((Translatable) b);
    }
    GameModule.getGameModule().getGameState().addGameSetupStep(this);
  }

  @Override
  public void build(Element e) {
    if (e == null) {
      final Board b = new Board();
      b.build(null);
      b.addTo(this);
    }
    else {
      final NodeList l = e.getElementsByTagName(SETUP);
      if (l.getLength() > 0) {
        final Element setupEl = (Element) l.item(0);
        defaultSetup = Builder.getText(setupEl);
        final Node nextSibling = setupEl.getNextSibling();
        e.removeChild(setupEl);
        Builder.build(e, this);
        e.insertBefore(setupEl, nextSibling);
      }
      else {
        Builder.build(e, this);
      }

      try {
        psize = new Dimension(Integer.parseInt(e.getAttribute(SLOT_WIDTH)),
                              Integer.parseInt(e.getAttribute(SLOT_HEIGHT)));
      }
      catch (NumberFormatException ex) {
        // Use default values if attribute doesn't parse.
        // Correct value will be written when module is saved.
      }

      try {
        slotScale = Double.parseDouble(e.getAttribute(SCALE));
      }
      catch (NumberFormatException ex) {
        // Use default values if attribute doesn't parse.
        // Correct value will be written when module is saved.
      }

      try {
        maxColumns = Integer.parseInt(e.getAttribute(MAX_COLUMNS));
      }
      catch (NumberFormatException ex) {
        // Use default values if attribute doesn't parse.
        // Correct value will be written when module is saved.
        maxColumns = 0;
      }

      String value = e.getAttribute(DIALOG_TITLE);
      if (value != null && value.length() > 0) {
        title = value;
      }
      /*
       * 'Add Row' and 'Add Column' text are no longer configurable, just use the standard (possibly translated) text
       *
       * value = e.getAttribute(ADD_ROW_BUTTON_TEXT); if (value != null && value.length() > 0) { addRowButtonText =
       * value; } value = e.getAttribute(ADD_COLUMN_BUTTON_TEXT); if (value != null && value.length() > 0) {
       * addColumnButtonText = value; }
       */
      value = e.getAttribute(BOARD_PROMPT);
      if (value != null && value.length() > 0) {
        boardPrompt = value;
      }
      // Record attributes for later translation
      Localization.getInstance().saveTranslatableAttribute(this, BOARD_PROMPT, boardPrompt);
      Localization.getInstance().saveTranslatableAttribute(this, DIALOG_TITLE, title);
    }
  }

  @Override
  public void validate(Buildable target, ValidationReport report) {
    if (possibleBoards.isEmpty()) {
      report.addWarning(Resources.getString("BoardPicker.must_define", ConfigureTree.getConfigureName(map))); //$NON-NLS-1$
    }
    final HashSet<String> names = new HashSet<>();
    for (final Board b : possibleBoards) {
      if (names.contains(b.getName())) {
        report.addWarning(Resources.getString("BoardPicker.more_than_one", b.getName(), ConfigureTree.getConfigureName(map))); //$NON-NLS-1$
      }
      names.add(b.getName());
      if (b.getName() == null) {
        report.addWarning(Resources.getString("BoardPicker.no_name", ConfigureTree.getConfigureName(map)));
      }
      b.validate(b, report);
    }
  }

  private String getDefaultSetup() {
    String s = defaultSetup;
    if (defaultSetup == null || defaultSetup.length() == 0) {
      if (possibleBoards.size() == 1) {
        final Board b = possibleBoards.get(0);
        if (!"true".equals(b.getAttributeValueString(Board.REVERSIBLE))) { //$NON-NLS-1$
          s = encode(new SetBoards(this, Collections.singletonList(b)));
        }
      }
    }
    return s;
  }

  /**
   * Add a board to the list of those available for the user to choose from
   */
  @Override
  public void add(Buildable b) {
    if (b instanceof Board) {
      possibleBoards.add((Board) b);
    }
    if (b instanceof Translatable) {
      ((Translatable) b).getI18nData().setOwningComponent(this);
    }
    super.add(b);  // PG-2011-09-24
  }

  /**
   * Remove a board from the list of those available for the user to choose from
   */
  @Override
  public void remove(Buildable b) {
    buildComponents.remove(b);  // PG-2011-09-24
    if (b instanceof Board) {
      possibleBoards.remove(b);
    }
  }

  @Override
  public void removeFrom(Buildable parent) {
    GameModule.getGameModule().getGameState().removeGameSetupStep(this);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.BoardPicker.component_type"); //$NON-NLS-1$
  }

  @Override
  public String getConfigureName() {
    return null;
  }

  public String getBoardDelimiter() {
    return "bd\t"; //$NON-NLS-1$
  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public Configurer getConfigurer() {
    return new Config();
  }

  @Override
  public Configurable[] getConfigureComponents() {
    return possibleBoards.toArray(new Configurable[0]);
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{Board.class};
  }

  @Override
  public void addPropertyChangeListener(java.beans.PropertyChangeListener l) {
  }

  public void setBoards(Collection<Board> c) {
    reset();
    for (final Board b : c) {
      if (b.relativePosition().x > nx - 1)
        addColumn();
      if (b.relativePosition().y > ny - 1)
        addRow();
    }
    for (final Board b : c) {
      getSlot(b.relativePosition().x + nx * b.relativePosition().y).setBoard(b);
    }
  }

  /** @deprecated Use {@link #setBoards(Collection)} instead. */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void setBoards(Enumeration<Board> bdEnum) {
    ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
    setBoards(Collections.list(bdEnum));
  }

  protected void selectBoards(Component c) {
    reset();
    final JDialog d = new JDialog(GameModule.getGameModule().getPlayerWindow(), true);
    d.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    final Box b = Box.createVerticalBox();
    final Box buttons = Box.createHorizontalBox();
    final JButton ok = new JButton(Resources.getString(Resources.OK));
    ok.addActionListener(e -> {
      final List<Board> l = getBoardsFromControls();
      defaultSetup = l.isEmpty() ? null : encode(new SetBoards(this, l));
      d.dispose();
    });
    buttons.add(ok);
    final JButton cancel = new JButton(Resources.getString(Resources.CANCEL));
    cancel.addActionListener(e -> d.dispose());
    buttons.add(cancel);
    b.add(controls);
    b.add(buttons);
    d.add(b);
    SwingUtils.repack(d);
    d.setLocationRelativeTo(c);
    d.setVisible(true);
    currentBoards = new ArrayList<>(getBoardsFromControls());
  }

  /**
   * @return a Collection of boards that have been selected either by the user via the dialog or from reading a savefile
   */
  public Collection<Board> getSelectedBoards() {
    if (currentBoards == null) {
      return Collections.emptyList();
    }
    return Collections.unmodifiableCollection(currentBoards);
  }

  /**
   * @return a List of the names of all boards from which have been selected either by the user via the dialog or from reading a savefile
   */
  public List<String> getSelectedBoardNames() {
    if (currentBoards == null) {
      return Collections.emptyList();
    }
    return currentBoards.stream().map(Board::getName).collect(Collectors.toList());
  }

  /**
   * @return an Enumeration of boards that have been selected either by the user via the dialog or from reading a
   *         savefile
   * @deprecated Use {@link #getSelectedBoards()} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Enumeration<Board> getCurrentBoards() {
    ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
    return Collections.enumeration(getSelectedBoards());
  }

  /**
   * @return an array of the names of all boards from which the user may choose
   */
  public String[] getAllowableBoardNames() {
    final ArrayList<String> s = new ArrayList<>(possibleBoards.size());
    for (final Board b : possibleBoards) {
      s.add(b.getName());
    }
    return s.toArray(new String[0]);
  }

  public String[] getAllowableLocalizedBoardNames() {
    final ArrayList<String> s = new ArrayList<>(possibleBoards.size());
    for (final Board b : possibleBoards) {
      s.add(b.getLocalizedName());
    }
    return s.toArray(new String[0]);
  }

  /**
   * @return a Board with the given name.
   */
  public Board getBoard(String boardName) {
    return getBoard(boardName, false);
  }

  public Board getLocalizedBoard(String localizedBoardName) {
    return getBoard(localizedBoardName, true);
  }

  protected Board getBoard(String boardName, boolean localized) {
    for (final Board b : possibleBoards) {
      final String checkName = localized ? b.getLocalizedName() : b.getName();
      if (checkName.equals(boardName)) {
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
  @Override
  public void setup(boolean show) {
    if (show) {
      if (currentBoards == null) {
        final String setup = getDefaultSetup();
        if (setup != null) {
          final Command c = decode(setup);
          if (c != null) {
            c.execute();
          }
        }
      }
      map.setBoards(getSelectedBoards());
    }
    else {
      currentBoards = null;
    }
  }

  @Override
  public void finish() {
    currentBoards = new ArrayList<>(getBoardsFromControls());
    map.setBoards(getSelectedBoards());
  }

  @Override
  public Component getControls() {
    reset();
    return controls;
  }

  @Override
  public String getStepTitle() {
    return title;
  }

  @Override
  public boolean isFinished() {
    return currentBoards != null || getDefaultSetup() != null;
  }

  /**
   * The restore command of a BoardPicker, when executed, sets the boards of its {@link Map} to
   * {@link #getSelectedBoards}
   */
  @Override
  public Command getRestoreCommand() {
    return new SetBoards(this, currentBoards);
  }

  protected JButton addButton(String s) {
    return addButton(s, -1);
  }

  protected JButton addButton(String s, int index) {
    final JButton b = new JButton(s);
    b.addActionListener(this);
    toolbar.add(b, null, index);
    return b;
  }

  protected void addRow() {
    slotPanel.setLayout(new GridLayout(++ny, nx));
    for (int i = 0; i < nx; ++i) {
      slotPanel.add(new BoardSlot(this, boardPrompt), -1);
    }
    slotPanel.revalidate();
  }

  protected void addColumn() {
    slotPanel.setLayout(new GridLayout(ny, ++nx));
    for (int j = 0; j < ny; ++j) {
      slotPanel.add(new BoardSlot(this, boardPrompt), (j + 1) * nx - 1);
    }
    slotPanel.revalidate();
  }

  @Override
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
  }

  /**
   * @deprecated Use {@link #getBoardsFromControls()}.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Vector<Board> pickBoards() { //NOPMD
    ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
    return new Vector<>(getBoardsFromControls());
  }

  /**
   * Return the list of boards as specified in the current controls
   *
   * @return List of Boards
   */
  public List<Board> getBoardsFromControls() {
    final ArrayList<Board> boardList = new ArrayList<>();
    if (toolbar != null) {
      // Adjust the bounds of each board according to its relative position
      for (int i = 0; i < nx; ++i) {
        for (int j = 0; j < ny; ++j) {
          final BoardSlot slot = getSlot(i + nx * j);
          if (slot != null) {
            final Board b = slot.getBoard();
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
    if (toolbar == null) {
      initComponents();
    }
    else {
      warn(""); //$NON-NLS-1$
      removeAllBoards();
      slotPanel.add(new BoardSlot(this, boardPrompt), 0);
    }
    controls.revalidate();
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
        if (x >= 0) {
          break;
        }
      }
    }
    x += dx;
    y += dy;
    if (x < 0 || x >= nx || y < 0 || y >= ny) {
      return null;
    }
    final int index = x + y * nx;
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
      for (final JButton b : multipleButtons) {
        b.setVisible(allowMultiple);
      }
    }
  }

  @Override
  public Element getBuildElement(Document doc) {
    final Element el = doc.createElement(getClass().getName());
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
      final Element setupEl = doc.createElement(SETUP);
      setupEl.appendChild(doc.createTextNode(defaultSetup));
      el.appendChild(setupEl);
    }
    for (final Board b : possibleBoards) {
      el.appendChild(b.getBuildElement(doc));
    }
    return el;
  }

  @Override
  public Command decode(String command) {
    if (!command.startsWith(map.getId() + ID + "\t") &&
      !command.startsWith(map.getConfigureName() + ID + "\t")) {
      return null;
    }

    final List<Board> bds = new ArrayList<>();
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, '\t');
    st.nextToken();
    while (st.hasMoreTokens()) {
      final SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(st.nextToken(), '/');
      final String name = st2.nextToken();
      boolean reversed = false;
      if (st2.hasMoreTokens()) {
        reversed = "rev".equals(st2.nextToken()); //$NON-NLS-1$
      }
      final Point p = new Point(st.nextInt(0), st.nextInt(0));
      Board b = getBoard(name);
      if (b != null) {
        if (bds.contains(b)) {
          b = b.copy();
        }
        b.setReversed(reversed);
        b.relativePosition().move(p.x, p.y);
        bds.add(b);
      }
    }
    return new SetBoards(this, bds);
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof SetBoards) || map == null || ((SetBoards) c).target != this) {
      return null;
    }

    final SequenceEncoder se =
      new SequenceEncoder(map.getIdentifier() + ID, '\t');
    final List<Board> bds = ((SetBoards) c).boards;
    if (bds == null) {
      return se.getValue();
    }

    for (final Board b : bds) {
      if (b.getName() != null) {
        final SequenceEncoder se2 = new SequenceEncoder(b.getName(), '/');
        if (b.isReversed()) {
          se2.append("rev"); //$NON-NLS-1$
        }
        se.append(se2.getValue())
          .append(b.relativePosition().x)
          .append(b.relativePosition().y);
      }
    }
    return se.getValue();
  }

  public static class SetBoards extends Command {
    private final BoardPicker target;
    private final List<Board> boards;

    public SetBoards(BoardPicker picker, List<Board> bds) {
      target = picker;
      boards = bds;
    }

    /** @deprecated Use {@link #SetBoards(BoardPicker,List)}. */
    @Deprecated(since = "2020-08-06", forRemoval = true)
    public SetBoards(BoardPicker target, Vector<Board> boards) { //NOPMD
      ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
      this.target = target;
      this.boards = boards;
    }

    @Override
    protected void executeCommand() {
      target.currentBoards = boards;
      if (GameModule.getGameModule().getGameState().isGameStarted()) {
        target.map.setBoards(target.getSelectedBoards());
        target.map.getView().revalidate();
      }
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }

  private class Config extends Configurer {
    private final ComponentConfigPanel controls;

    public Config() {
      super(null, null);
      controls = new ComponentConfigPanel();


      final StringConfigurer title = new StringConfigurer(BoardPicker.this.title);
      title.addPropertyChangeListener(evt -> {
        if (evt.getNewValue() != null) {
          BoardPicker.this.title = (String) evt.getNewValue();
        }
      });
      controls.add("Editor.BoardPicker.dialog_title", title);

      final StringConfigurer prompt = new StringConfigurer(BoardPicker.this.boardPrompt);
      prompt.addPropertyChangeListener(evt -> {
        if (evt.getNewValue() != null) {
          BoardPicker.this.boardPrompt = (String) evt.getNewValue();
        }
      });
      controls.add("Editor.BoardPicker.board_prompt", prompt);

      final DoubleConfigurer scale = new DoubleConfigurer(slotScale);
      scale.addPropertyChangeListener(evt -> {
        if (evt.getNewValue() != null) {
          slotScale = (Double) evt.getNewValue();
        }
      });
      controls.add("Editor.BoardPicker.cell_scale_factor", scale);

      final IntConfigurer width = new IntConfigurer(psize.width);
      width.addPropertyChangeListener(evt -> {
        if (evt.getNewValue() != null) {
          psize.width = (Integer) evt.getNewValue();
        }
      });
      controls.add("Editor.BoardPicker.cell_width", width);

      final IntConfigurer height = new IntConfigurer(psize.height);
      height.addPropertyChangeListener(evt -> {
        if (evt.getNewValue() != null) {
          psize.height = (Integer) evt.getNewValue();
        }
      });
      controls.add("Editor.BoardPicker.cell_height", height);

      final JButton selectButton = new JButton(Resources.getString("BoardPicker.select_default")); //$NON-NLS-1$
      selectButton.addActionListener(e -> selectBoards(e.getSource() instanceof Component ? (Component) e.getSource() : null));
      controls.add(selectButton, "skip 1,grow 0"); // NON-NLS
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getValueString() {
      return null;
    }

    @Override
    public void setValue(String s) {
    }
  }

  /*
   * Record which attributes are translatable
   */
  @Override
  public ComponentI18nData getI18nData() {
    if (myI18nData == null) {
      myI18nData = new ComponentI18nData(this, "", null, //$NON-NLS-1$
          new String[]{DIALOG_TITLE, BOARD_PROMPT}, new boolean[]{true, true},
          new String[]{Resources.getString("Editor.BoardPicker.dialog_title"), Resources.getString("Editor.BoardPicker.board_prompt") //$NON-NLS-1$ //$NON-NLS-2$
          });
    }
    return myI18nData;
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (DIALOG_TITLE.equals(key)) {
      title = (String) value;
    }
    else if (BOARD_PROMPT.equals(key)) {
      boardPrompt = (String) value;
    }
  }

  @Override
  public String getAttributeValueString(String attr) {
    if (DIALOG_TITLE.equals(attr)) {
      return title;
    }
    else if (BOARD_PROMPT.equals(attr)) {
      return boardPrompt;
    }
    return null;
  }

  public void repaint() {
    if (controls != null) {
      controls.repaint();
    }
  }

  /**
   * @deprecated No replacement
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void pack() {
    ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
  }

  @Override  // PG-2011-09-24
  public String[] getAttributeNames() {
    return new String[0];
  }
}
