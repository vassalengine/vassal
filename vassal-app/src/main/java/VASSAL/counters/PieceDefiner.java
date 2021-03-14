/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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
package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.GpIdSupport;
import VASSAL.build.module.KeyNamer;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.module.documentation.HelpWindowExtension;
import VASSAL.build.widget.PieceSlot;
import VASSAL.configure.IntConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ProblemDialog;
import VASSAL.tools.ReflectionUtils;
import VASSAL.tools.icon.IconFactory;
import VASSAL.tools.icon.IconFamily;
import VASSAL.tools.image.LabelUtils;
import VASSAL.tools.swing.SwingUtils;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DragSource;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.swing.BorderFactory;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.DropMode;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.TransferHandler;
import javax.swing.plaf.SplitPaneUI;
import javax.swing.plaf.basic.BasicSplitPaneUI;

import net.miginfocom.swing.MigLayout;

/**
 * This is the GamePiece designer dialog.  It appears when you edit
 * the properties of a "Single Piece" in the Configuration window.
 */
public class PieceDefiner extends JPanel implements HelpWindowExtension {
  private static final long serialVersionUID = 1L;

  // A Preference for holding the maximum split size between the image and the trait list panel.
  // The preference is set when the user manually drags the splitter bar
  private static final String MAX_SPLIT_PREF = "PieceDefinerMaxSplit"; // NON-NLS

  // Identifying names for the two trait lists to allow them to share resources
  private static final String AVAILABLE = "Available"; // NON-NLS
  private static final String INUSE = "InUse"; // NON-NLS

  // A reduced inset size for the icon buttons gives a better look
  private static final Insets buttonInsets = new Insets(1, 2, 1, 2);

  // Some empty space around the rendered Piece Image so it doesn't look to crammed
  private static final int PIECE_IMAGE_INSET = 10;

  protected static DefaultListModel<GamePiece> availableModel;
  private static final SortedMap<String, GamePiece> alphaMap = new TreeMap<>();
  private static Boolean sorted = false;
  protected DefaultListModel<GamePiece> inUseModel;
  protected ListCellRenderer<? super GamePiece> r;
  protected ScaleablePieceSlot slot;
  private GamePiece piece;
  protected static TraitClipboard clipBoard;
  protected String pieceId = "";
  protected JLabel pieceIdLabel = new JLabel("");
  protected GpIdSupport gpidSupport;
  protected boolean changed;

  protected JList<GamePiece> availableList;
  private JButton helpButton;
  private JButton addButton;
  private JButton removeButton;
  private JList<GamePiece> inUseList;
  private JButton propsButton;
  private JButton moveUpButton;
  private JButton moveDownButton;
  private JButton moveTopButton;
  private JButton moveBottomButton;
  protected JButton copyButton;
  protected JButton pasteButton;
  private JPanel controls;
  private JSplitPane splitPane;
  private JPanel slotPanel;
  private int maxPanelHeight;
  private JLabel scaleLabel;
  private final Prefs prefs;

  // A Configurer to hold the users preferred maximum split size
  private static final int MINIMUM_SPLIT_SIZE = LabelUtils.noImageBoxImage().getWidth();
  private static final int DEFAULT_MAX_SPLIT = 256;
  private boolean splitDragInProgress = false;

  /** Creates new form test */
  public PieceDefiner() {
    prefs = GameModule.getGameModule().getPrefs();
    final IntConfigurer maxSplitConfig = new IntConfigurer(MAX_SPLIT_PREF, null, DEFAULT_MAX_SPLIT);
    prefs.addOption(MAX_SPLIT_PREF, maxSplitConfig);

    initDefinitions();
    inUseModel = new DefaultListModel<>();
    r = new Renderer();
    slot = new ScaleablePieceSlot();
    initComponents();
    availableList.setSelectedIndex(0);
    setChanged(false);
    gpidSupport = GameModule.getGameModule().getGpIdSupport();
  }

  public PieceDefiner(String id, GpIdSupport s) {
    this();
    pieceId = id;
    pieceIdLabel.setText(Resources.getString("Editor.id") + ": " + id);
    gpidSupport = s;
  }

  public PieceDefiner(GpIdSupport s) {
    this();
    gpidSupport = s;
  }

  protected static void addElement(GamePiece piece) {
    // Add piece to the standard model
    availableModel.addElement(piece);

    // Store the piece in a SortedMap, order by translated trait description
    alphaMap.put(((EditablePiece) piece).getDescription(), piece);
  }


  protected static void initDefinitions() {
    if (availableModel == null) {
      availableModel = new DefaultListModel<>();
      //addElement(new BasicPiece()); // Not needed since you can never add it
      addElement(new Delete());
      addElement(new Clone());
      addElement(new Embellishment());
      addElement(new UsePrototype());
      addElement(new Labeler());
      addElement(new ReportState());
      addElement(new TriggerAction());
      addElement(new GlobalHotKey());
      addElement(new ActionButton());
      addElement(new FreeRotator());
      addElement(new Pivot());
      addElement(new Hideable());
      addElement(new Obscurable());
      addElement(new SendToLocation());
      addElement(new CounterGlobalKeyCommand());
      addElement(new Translate());
      addElement(new ReturnToDeck());
      addElement(new Immobilized());
      addElement(new PropertySheet());
      addElement(new TableInfo());
      addElement(new PlaceMarker());
      addElement(new Replace());
      addElement(new NonRectangular());
      addElement(new PlaySound());
      addElement(new MovementMarkable());
      addElement(new Footprint());
      addElement(new AreaOfEffect());
      addElement(new SubMenu());
      addElement(new MenuSeparator());
      addElement(new RestrictCommands());
      addElement(new Restricted());
      addElement(new Marker());
      addElement(new DynamicProperty());
      addElement(new CalculatedProperty());
      addElement(new SetGlobalProperty());
      addElement(new Deselect());
      addElement(new TranslatableMessage());

      // Generate a model sorted by description, in the current users language
      buildAlphaModel();
    }
  }

  private static void buildAlphaModel() {
    final DefaultListModel<GamePiece> alphaModel = new DefaultListModel<>();
    alphaMap.values().forEach(alphaModel::addElement);
    availableModel = alphaModel;
  }

  /**
   * Add an additional definition to the list of available traits.
   * Add to the bottom of the classic list
   * Regenerate the Alpha list
   * reset the model depending on the sort setting
   * @param piece New Definition
   */
  private static void addAdditionalElement(GamePiece piece) {
    initDefinitions();
    addElement(piece);
    buildAlphaModel();
  }

  /**
   * Plugins can add additional GamePiece definitions
   * @param definition GamePiece definition to add
   */
  public static void addDefinition(GamePiece definition) {
    addAdditionalElement(definition);
  }

  public static Boolean getSorted() {
    return sorted;
  }

  public static void setSorted(Boolean sorted) {
    PieceDefiner.sorted = sorted;
  }

  public void setPiece(GamePiece piece) {
    inUseModel.clear();
    while (piece instanceof Decorator) {
      final Class<?> pieceClass = piece.getClass();

      inUseModel.insertElementAt(piece, 0);
      boolean contains = false;
      final int j = availableModel.size();
      for (int i = 0; i < j; ++i) {
        if (pieceClass.isInstance(availableModel.elementAt(i))) {
          contains = true;
          break;
        }
      }

      if (!contains) {
        try {
          addAdditionalElement((GamePiece) pieceClass.getConstructor().newInstance());
        }
        catch (Throwable t) {
          ReflectionUtils.handleNewInstanceFailure(t, pieceClass);
        }
      }

      piece = ((Decorator) piece).piece;
    }

    inUseModel.insertElementAt(piece == null ? new BasicPiece() : piece, 0);
    inUseList.setSelectedIndex(0);

    refresh();
  }

  @Override
  @Deprecated(since = "2020-09-12", forRemoval = true)
  public void setBaseWindow(HelpWindow w) {
    ProblemDialog.showDeprecated("2020-09-12");
  }

  /**
   * The piece defined has been changed. It may have changed size, or image
   *
   */
  private void refresh() {
    if (inUseModel.getSize() > 0) {
      piece = inUseModel.lastElement();
    }
    else {
      piece = null;
    }
    slot.setPiece(piece);

    resizeSlotPanel();

    controls.revalidate();
    slotPanel.revalidate();
    slot.getComponent().repaint();
  }

  /**
   *
   * Re-calculate how to layout the left hand Piece display panel.
   *
   * 1. Always try and show the entire image 100% scale, plus PIECE_IMAGE_BORDER pixels
   * 2. If the image can't fit in the available space, scale it until it can
   * 3. If the image size is < max split size, then set the divider size = image size to now waste space
   *
   */
  private void resizeSlotPanel() {

    Dimension newSlotSize;

    // Find the size of the piece rendered at 100% in the PieceSlot.
    // If the PieceSlot has not been rendered yet, use the bounding box
    // of the piece as an estimate
    if (slot.isValid()) {
      // Reset the piece to 100% scale to get the actual size.
      final double oldScale = slot.getScale();
      slot.setScale(1.0);
      newSlotSize = slot.getPreferredSize();
      slot.setScale(oldScale);
    }
    else {
      slot.getComponent().revalidate();
      slot.getComponent().repaint();
      final Rectangle r = slot.getPiece().boundingBox();
      newSlotSize = new Dimension(r.width, r.height);
    }

    final int maxSlotWidth = getMaxSplit() - PIECE_IMAGE_INSET * 2;
    final int maxSlotHeight = maxPanelHeight - PIECE_IMAGE_INSET * 2;

    // Updated image size, work out a new scale factor and divider position to
    // best display it.
    if (newSlotSize.width <= maxSlotWidth && newSlotSize.height <= maxSlotHeight) {
      // The new piece will fit at 100% scale. Set the maximum size of the slotPanel to prevent it expanding too far
      slotPanel.setMaximumSize(new Dimension(newSlotSize.width + PIECE_IMAGE_INSET * 2, maxPanelHeight));
      final Dimension updatedSlotSize = new Dimension(newSlotSize.width + PIECE_IMAGE_INSET * 2, newSlotSize.height);
      slotPanel.setPreferredSize(updatedSlotSize);
      slot.setScale(1.0);
      scaleLabel.setText("");
    }
    else {
      // The new piece is too large to fit in the available space. Scale it to fit on the most oversized axis
      final double xRatio = maxSlotWidth / newSlotSize.getWidth();
      final double yRatio = maxSlotHeight / newSlotSize.getHeight();
      final double newScale = Math.min(xRatio, yRatio);
      slot.setScale(newScale);
      scaleLabel.setText("(" + (int) (newScale * 100) + "%)");
      slot.getComponent().revalidate();
      slot.getComponent().repaint();
      final Dimension scaledSlotSize = new Dimension((int) (newSlotSize.width * newScale), (int) (newSlotSize.height * newScale));
      slotPanel.setMaximumSize(new Dimension(Math.min(scaledSlotSize.width + PIECE_IMAGE_INSET * 2, maxSlotWidth), maxPanelHeight));
      final Dimension updatedSlotSize = new Dimension(scaledSlotSize.width + PIECE_IMAGE_INSET * 2, scaledSlotSize.height);
      slotPanel.setPreferredSize(updatedSlotSize);
    }
    slotPanel.revalidate();
    splitPane.resetToPreferredSizes();
    slot.getComponent().repaint();
  }

  public GamePiece getPiece() {
    return piece;
  }

  public void setChanged(boolean b) {
    changed = b;
  }

  public boolean isChanged() {
    return changed;
  }

  /**
   * This method is called from within the constructor to initialize the form.
   */
  private void initComponents() {
    maxPanelHeight = SwingUtils.getScreenSize().height - 150;

    // Main Layout for entire dialog. Just let it fill to max size
    setLayout(new MigLayout("ins 0, fill")); // NON-NLS

    // A Panel to hold the trait lists
    controls = new JPanel(new MigLayout("ins 0, fill", "[grow 1,:200:]rel[]rel[grow 4,:400:]rel[]", "[grow][][]")); // NON-NLS

    // A Panel to hold the generated PieceSlot image
    slotPanel = new JPanel(new MigLayout("ins 0", "push[]push", "push[]2[]push")); // NON-NLS
    slotPanel.setMinimumSize(new Dimension(MINIMUM_SPLIT_SIZE, MINIMUM_SPLIT_SIZE));
    slotPanel.add(slot.getComponent(), "grow,wrap"); // NON-NLS
    scaleLabel = new JLabel();
    slotPanel.add(scaleLabel, "center,aligny top"); // NON-NLS

    // Split the screen horizontally. PieceSlot image on the left, trait lists on the right.
    splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, slotPanel, controls);
    splitPane.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, e -> splitChanged());

    // Set a MouseListener on the Divider so we can distinguish manual drags from auto-resizes
    final SplitPaneUI spui = splitPane.getUI();
    if (spui instanceof BasicSplitPaneUI) {
      ((BasicSplitPaneUI) spui).getDivider().addMouseListener(new MouseAdapter() {
        @Override
        public void mousePressed(MouseEvent e) {
          super.mousePressed(e);
          setSplitDragInProgress(true);
        }

        @Override
        public void mouseReleased(MouseEvent e) {
          super.mouseReleased(e);
          setSplitDragInProgress(false);
        }
      });
    }

    add(splitPane, "grow,pushy"); // NON-NLS

    // Custom TransferHandler to mediate drag and drop
    final ListTransferHandler transferHandler = new ListTransferHandler(this);

    // The list of available Traits
    final JPanel availablePanel = new JPanel(new MigLayout("ins 0, fill")); // NON-NLS
    availableList = new JList<>();
    availableList.setName(AVAILABLE);
    availableList.setDragEnabled(true);
    availableList.setTransferHandler(transferHandler);
    availableList.setModel(availableModel);
    availableList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    availableList.setCellRenderer(r);
    availableList.addKeyListener(new AvailableListKeyAdapter(this));
    availableList.setVisibleRowCount(99);
    availableList.addListSelectionListener(evt -> {
      final Object o = availableList.getSelectedValue();
      helpButton.setEnabled(o instanceof EditablePiece && ((EditablePiece) o).getHelpFile() != null);
      addButton.setEnabled(o instanceof Decorator);
    });
    availableList.addMouseListener(new MouseAdapter() {
      @Override
      public void mouseClicked(MouseEvent e) {
        if (e.getClickCount() == 2) {
          doAdd();
        }
      }
    });

    final JPanel availableListPanel = new JPanel(new MigLayout("ins 0, fill")); // NON-NLS
    availableListPanel.add(availableList, "grow,push"); // NON-NLS
    final JScrollPane availableScroll = new JScrollPane(availableListPanel);

    availableScroll.setBorder(BorderFactory.createTitledBorder(Resources.getString("Editor.PieceDefiner.available_traits")));
    availablePanel.add(availableScroll, "grow,push"); // NON-NLS

    // A Panel to hold the Help and Import button below the Available Traits
    final JPanel availableButtonPanel = new JPanel(new MigLayout("ins 0", "push[]rel[]rel[]push")); // NON-NLS

    helpButton = new JButton(Resources.getString("General.help"));
    helpButton.setToolTipText(Resources.getString("Editor.PieceDefiner.help_tip"));
    helpButton.addActionListener(evt -> showHelpForPiece());

    final JButton importButton = new JButton(Resources.getString("General.import"));
    importButton.setToolTipText(Resources.getString("Editor.PieceDefiner.import_tip"));
    importButton.addActionListener(evt -> {
      final String className = JOptionPane.showInputDialog(
        this, Resources.getString("Editor.PieceDefiner.enter_class")
      );
      importPiece(className);
    });

    availableButtonPanel.add(importButton, "sg 2"); // NON-NLS
    availableButtonPanel.add(helpButton, "sg 2"); // NON-NLS
    controls.add(availablePanel, "grow,pushy"); // NON-NLS

    // A Panel holding the add and Remove buttons between the two trait lists
    final JPanel addRemovePanel = new JPanel();
    addRemovePanel.setLayout(new MigLayout("ins 0,wrap 1")); // NON-NLS

    addButton = new JButton(Resources.getString("Editor.PieceDefiner.add"), IconFactory.getIcon("go-next", IconFamily.SMALL)); // NON-NLS
    addButton.setHorizontalTextPosition(SwingUtilities.LEFT);
    addButton.setMargin(buttonInsets);
    addButton.setToolTipText(Resources.getString("Editor.PieceDefiner.add_tip"));
    addButton.addActionListener(evt -> doAdd());
    addRemovePanel.add(addButton, "sg"); // NON-NLS

    removeButton = new JButton(Resources.getString("Editor.PieceDefiner.remove"), IconFactory.getIcon("go-previous", IconFamily.SMALL)); // NON-NLS
    removeButton.setMargin(buttonInsets);
    removeButton.setToolTipText(Resources.getString("Editor.PieceDefiner.remove_tip"));
    removeButton.addActionListener(evt -> doRemove());
    addRemovePanel.add(removeButton, "sg"); // NON-NLS
    addRemovePanel.add(pieceIdLabel, "center"); // NON-NLS

    controls.add(addRemovePanel, "aligny center"); // NON-NLS

    // The list of traits in use in this GamePiece
    final JPanel inUsePanel = new JPanel(new MigLayout("ins 0,wrap 1,fill")); // NON-NLS
    inUseList = new JList<>();
    inUseList.setName(INUSE);
    inUseList.addKeyListener(new InUseListKeyAdapter(this));
    inUseList.setDragEnabled(true);
    inUseList.setDropMode(DropMode.INSERT);
    inUseList.setTransferHandler(transferHandler);
    inUseList.setVisibleRowCount(99);
    inUseList.setModel(inUseModel);
    inUseList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    inUseList.setCellRenderer(r);
    inUseList.addListSelectionListener(evt -> {
      final Object o = inUseList.getSelectedValue();
      propsButton.setEnabled(o instanceof EditablePiece);

      final int index = inUseList.getSelectedIndex();
      final boolean copyAndRemove = inUseModel.size() > 0 &&
        (index > 0 || !(inUseModel.getElementAt(0) instanceof BasicPiece));
      copyButton.setEnabled(copyAndRemove);
      removeButton.setEnabled(copyAndRemove);

      pasteButton.setEnabled(clipBoard != null);
      moveUpButton.setEnabled(index > 1);
      moveTopButton.setEnabled(index > 1);
      moveDownButton.setEnabled(index > 0 && index < inUseModel.size() - 1);
      moveBottomButton.setEnabled(index > 0 && index < inUseModel.size() - 1);
    });

    inUseList.addMouseListener(new MouseAdapter() {
      @Override
// FIXME: mouseClicked()?
      public void mouseReleased(MouseEvent e) {
        if (e.getClickCount() == 2 && SwingUtils.isMainMouseButtonDown(e)) {
          final int index = inUseList.locationToIndex(e.getPoint());
          if (index >= 0) {
            edit(index);
          }
        }
      }
    });
    final JPanel inUseListPanel = new JPanel(new BorderLayout());
    inUseListPanel.add(inUseList, BorderLayout.CENTER);
    final JScrollPane inUseScroll = new JScrollPane(inUseListPanel);
    inUseScroll.setBorder(BorderFactory.createTitledBorder(Resources.getString("Editor.PieceDefiner.current_traits")));
    inUsePanel.add(inUseScroll, "grow"); // NON-NLS

    // A panel holding the cut/paste/propertiues buttons below the In Use traits
    final JPanel inUseButtonPanel = new JPanel(new MigLayout("ins 0", "push[]rel[]rel[]push")); // NON-NLS
    copyButton = new JButton(Resources.getString("Editor.copy") + " (" + getCtrlKeyName('C') + ")");
    copyButton.addActionListener(evt -> doCopy());
    inUseButtonPanel.add(copyButton, "sg 1"); // NON-NLS

    pasteButton = new JButton(Resources.getString("Editor.paste") + " (" + getCtrlKeyName('V') + ")");
    pasteButton.setEnabled(clipBoard != null);
    pasteButton.addActionListener(evt -> doPaste());
    inUseButtonPanel.add(pasteButton, "sg 1"); // NON-NLS

    propsButton = new JButton(Resources.getString("Editor.properties"));
    propsButton.addActionListener(evt -> {
      final int index = inUseList.getSelectedIndex();
      if (index >= 0) {
        edit(index);
      }
    });

    inUseButtonPanel.add(propsButton, "sg 1"); // NON-NLS

    controls.add(inUsePanel, "grow"); // NON-NLS

    // A panel to hold the trait navigation buttons.
    final JPanel moveUpDownPanel = new JPanel(new MigLayout("ins 0,wrap 1")); // NON-NLS

    moveTopButton = new JButton(Resources.getString("Editor.PieceDefiner.move_top"), IconFactory.getIcon("go-top", IconFamily.SMALL)); // NON-NLS
    moveTopButton.setToolTipText(Resources.getString("Editor.PieceDefiner.top_tip"));
    moveTopButton.setHorizontalAlignment(SwingConstants.LEFT);
    moveTopButton.setMargin(buttonInsets);
    moveTopButton.addActionListener(evt -> {
      final int index = inUseList.getSelectedIndex();
      if (index > 1 && index < inUseModel.size()) {
        moveDecoratorTop(index);
      }
    });
    moveUpDownPanel.add(moveTopButton, "sg"); // NON-NLS

    moveUpButton = new JButton(Resources.getString("Editor.PieceDefiner.move_up"), IconFactory.getIcon("go-up", IconFamily.SMALL)); // NON-NLS
    moveUpButton.setToolTipText(Resources.getString("Editor.PieceDefiner.up_tip"));
    moveUpButton.setHorizontalAlignment(SwingConstants.LEFT);
    moveUpButton.setMargin(buttonInsets);
    moveUpButton.addActionListener(evt -> {
      final int index = inUseList.getSelectedIndex();
      if (index > 1 && index < inUseModel.size()) {
        moveDecoratorUp(index);
      }
    });
    moveUpDownPanel.add(moveUpButton, "sg"); // NON-NLS

    moveDownButton = new JButton(Resources.getString("Editor.PieceDefiner.move_down"), IconFactory.getIcon("go-down", IconFamily.SMALL)); // NON-NLS
    moveDownButton.setToolTipText(Resources.getString("Editor.PieceDefiner.down_tip"));
    moveDownButton.setHorizontalAlignment(SwingConstants.LEFT);
    moveDownButton.setMargin(buttonInsets);
    moveDownButton.addActionListener(evt -> {
      final int index = inUseList.getSelectedIndex();
      if (index > 0 && index < inUseModel.size() - 1) {
        moveDecoratorDown(index);
      }
    });
    moveUpDownPanel.add(moveDownButton, "sg"); // NON-NLS

    moveBottomButton = new JButton(Resources.getString("Editor.PieceDefiner.move_bottom"), IconFactory.getIcon("go-bottom", IconFamily.SMALL)); // NON-NLS
    moveBottomButton.setToolTipText(Resources.getString("Editor.PieceDefiner.bottom_trait"));
    moveBottomButton.setHorizontalAlignment(SwingConstants.LEFT);
    moveBottomButton.setMargin(buttonInsets);
    moveBottomButton.addActionListener(evt -> {
      final int index = inUseList.getSelectedIndex();
      if (index > 0 && index < inUseModel.size() - 1) {
        moveDecoratorBottom(index);
      }
    });
    moveUpDownPanel.add(moveBottomButton, "sg"); // NON-NLS

    controls.add(moveUpDownPanel, "wrap"); // NON-NLS

    controls.add(availableButtonPanel, "center"); // NON-NLS
    controls.add(new JLabel(""));
    controls.add(inUseButtonPanel, "center,wrap"); // NON-NLS

    // A label to hold the notes at the bottom of the screen
    final JLabel noteLabel = new JLabel(Resources.getString("Editor.PieceDefiner.note1"));
    final Font standardFont = noteLabel.getFont();
    final Font italicFont = new Font(standardFont.getFontName(), Font.ITALIC, standardFont.getSize());
    noteLabel.setFont(italicFont);

    controls.add(noteLabel, "span 3,center"); // NON-NLS

  }

  /**
   * The user has manually dragged the divider to change the split size.
   * Record the level as current maximum and resize the image if it is not
   * at 100% already
   */
  private void splitChanged() {
    // Exclude size changes due to resizes not initiated by the user
    if (! isSplitDragInProgress()) {
      return;
    }

    final int splitPosition = splitPane.getDividerLocation();

    // Save the new split position
    setMaxSplit(splitPosition);

    // Re-size the image if necessary
    resizeSlotPanel();
  }

  private int getMaxSplit() {
    return (Integer) prefs.getValue(MAX_SPLIT_PREF);
  }

  private void setMaxSplit(int split) {
    prefs.setValue(MAX_SPLIT_PREF, split);
  }

  public boolean isSplitDragInProgress() {
    return splitDragInProgress;
  }

  public void setSplitDragInProgress(boolean splitDragInProgress) {
    this.splitDragInProgress = splitDragInProgress;
  }

  /**
   * Return the OS specific name for a control Key.
   * This will usually be Ctrl+c or Cmd+c on a Mac for character c
   *
   * @param c Control Char
   * @return OS specific name.
   */
  private String getCtrlKeyName(char c) {
    return KeyNamer.getKeyString(SwingUtils.genericToSystem(KeyStroke.getKeyStroke(c, Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx())));
  }

  private void doCopy() {
    final int index = inUseList.getSelectedIndex();
    if (index >= 0) {
      pasteButton.setEnabled(true);
      clipBoard = new TraitClipboard((Decorator) inUseModel.get(index));
    }
  }

  private void doPaste() {
    if (clipBoard != null) {
      paste();
    }
  }

  private void doRemove() {
    final int index = inUseList.getSelectedIndex();
    if (index >= 0) {
      removeTrait(index);
      if (inUseModel.getSize() > 0) {
        inUseList.setSelectedIndex(Math.min(inUseModel.getSize() - 1, index));
      }
    }
  }

  private void doAdd() {
    doAdd(availableList.getSelectedIndex(), inUseList.getSelectedIndex() < 0 ? inUseList.getModel().getSize() : inUseList.getSelectedIndex());
  }

  private void doAdd(int sourceIndex, int insertIndex) {
    final Object selected = availableList.getModel().getElementAt(sourceIndex);
    if (selected instanceof Decorator) {
      if (inUseModel.getSize() > 0) {
        final Decorator c = (Decorator) selected;
        addTrait(c, insertIndex);
        if (inUseModel.getElementAt(insertIndex + 1).getClass() == c.getClass()) {
          final int previousSelect = inUseList.getSelectedIndex();
          if (!edit(insertIndex + 1)) {
            // Add was cancelled
            if (!inUseModel.isEmpty()) {
              removeTrait(insertIndex + 1);
              inUseList.setSelectedIndex(Math.min(previousSelect, inUseModel.getSize()) - 1);
            }
          }
        }
      }
    }
    else if (selected != null && inUseModel.getSize() == 0) {
      GamePiece p = null;
      try {
        p = (GamePiece) selected.getClass().getConstructor().newInstance();
      }
      catch (Throwable t) {
        ReflectionUtils.handleNewInstanceFailure(t, selected.getClass());
      }

      if (p != null) {
        setPiece(p);
        if (inUseModel.getSize() > 0) {
          if (!edit(0)) {
            // Add was cancelled
            removeTrait(0);
          }
        }
      }
    }
  }

  protected void paste() {
    final Decorator c = (Decorator) GameModule.getGameModule().createPiece(clipBoard.getType(), null);
    if (c instanceof PlaceMarker) {
      ((PlaceMarker) c).updateGpId(GameModule.getGameModule().getGpIdSupport());
    }
    final int selectedIndex = inUseList.getSelectedIndex();
    c.setInner(inUseModel.lastElement());
    inUseModel.addElement(c);
    c.mySetState(clipBoard.getState());
    moveDecorator(inUseModel.size() - 1, selectedIndex + 1);
    refresh();
  }

  protected void moveDecoratorDown(int index) {
    moveDecorator(index, index + 2); // Note +2, not +1 because moveDecorator subtracts 1 to take account of entries moving up
  }

  protected void moveDecoratorUp(int index) {
    moveDecorator(index, index - 1);
  }

  protected void moveDecoratorTop(int index) {
    moveDecorator(index, 1);
  }

  protected void moveDecoratorBottom(int index) {
    moveDecorator(index, inUseModel.size());
  }

  /**
   * Move a Decorator to a new position in the list
   *
   * @param fromIndex Old Position
   * @param toIndex New Position
   */
  protected void moveDecorator(int fromIndex, int toIndex) {

    // Can't move anything above BasicPiece
    if (inUseModel.size() > 0) {
      if (inUseModel.getElementAt(0) instanceof BasicPiece && toIndex == 0) {
        return;
      }
    }

    if (toIndex == fromIndex) {
      inUseList.setSelectedIndex(toIndex);
      return;
    }

    // Remove the piece from it's current position
    final GamePiece piece = removeDecorator(fromIndex);

    // Insert it at the new position.
    // The piece has already been removed from its old location, so if we are moving down,
    // substract one from the toIndex as the list entries below us will have shifted up.
    final int actualToIndex = toIndex > fromIndex ? toIndex - 1 : toIndex;
    insertDecorator(actualToIndex, piece);

    // Make the sure the outermost trait knows it is outermost
    inUseModel.lastElement().setProperty(Properties.OUTER, null);

    // Select the newly inserted piece
    inUseList.setSelectedIndex(actualToIndex);

    // Tidy up
    refresh();
    setChanged(true);
  }

  /**
   * Remove the Decorator at a given position in the list
   *
   * @param index Position of Decorator to remove
   * @return Removed Decorator
   */
  protected GamePiece removeDecorator(int index) {

    // Find the existing inner and outer of the piece to be removed
    final GamePiece inner = inUseModel.elementAt(index - 1);
    final Decorator outer = index < inUseModel.size() - 1 ?  (Decorator) inUseModel.elementAt(index + 1) : null;

    // Remove the piece at index
    final GamePiece piece = inUseModel.remove(index);

    // If this was not the outermost Decorator, set the inner of the outer to the new inner :)
    if (outer != null) {
      outer.setInner(inner);
    }

    // Return the removed piece
    return piece;
  }

  /**
   * Insert a Decorator into the list at a given position
   *
   * @param index Position to insert Decorator
   * @param piece Decorator to insert
   */
  protected void insertDecorator(int index, GamePiece piece) {
    // Find the pieces that will be the new inner and outer for our piece
    final GamePiece inner = inUseModel.elementAt(index - 1);
    final Decorator outer = index < inUseModel.size() ? (Decorator) inUseModel.elementAt(index) : null;

    // Insert the new piece
    inUseModel.insertElementAt(piece, index);

    // Update the inner pointers
    ((Decorator) piece).setInner(inner);
    if (outer != null) {
      outer.setInner(piece);
    }
  }

  protected void importPiece(String className) {
    if (className == null) return;

    Object o = null;
    try {
      o = GameModule.getGameModule().getDataArchive()
                    .loadClass(className).getConstructor().newInstance();
    }
    catch (Throwable t) {
      ReflectionUtils.handleImportClassFailure(t, className);
    }

    if (o == null) return;

    if (o instanceof GamePiece) {
      addAdditionalElement((GamePiece) o);
    }
    else {
      ErrorDialog.show("Error.not_a_gamepiece", className); // NON-NLS Error Dialog Key
    }
  }

  private void showHelpForPiece() {
    final Object o = availableList.getSelectedValue();
    if (o instanceof EditablePiece) {
      final HelpFile h = ((EditablePiece) o).getHelpFile();
      BrowserSupport.openURL(h.getContents().toString());
    }
  }

  protected boolean edit(int index) {
    final Object o = inUseModel.elementAt(index);
    if (!(o instanceof EditablePiece)) {
      return false;
    }
    final EditablePiece p = (EditablePiece) o;
    if (p.getEditor() != null) {
      final Ed ed;
      final Window w = SwingUtilities.getWindowAncestor(this);
      if (w instanceof Frame) {
        ed = new Ed((Frame) w, p);
      }
      else if (w instanceof Dialog) {
        ed = new Ed((Dialog) w, p);
      }
      else {
        ed = new Ed((Frame) null, p);
      }
      final String oldState = p.getState();
      final String oldType = p.getType();
      ed.setVisible(true);
      final PieceEditor c = ed.getEditor();
      if (c != null) {
        p.mySetType(c.getType());
        if (p instanceof Decorator) {
          ((Decorator) p).mySetState(c.getState());
        }
        else {
          p.setState(c.getState());
        }
        if ((! p.getType().equals(oldType)) || (! p.getState().equals(oldState))) {
          setChanged(true);
        }
        refresh();
        return true;
      }
    }
    return false;
  }

  /** A Dialog for editing an EditablePiece's properties */
  protected static class Ed extends JDialog {
    private static final long serialVersionUID = 1L;

    PieceEditor ed;

    private Ed(Frame owner, final EditablePiece p) {
      super(owner, Resources.getString("Editor.PieceDefiner.properties", p.getDescription()), true);
      initialize(p);
    }

    private Ed(Dialog owner, final EditablePiece p) {
      super(owner, Resources.getString("Editor.PieceDefiner.properties", p.getDescription()), true);
      initialize(p);
    }

    private void initialize(final EditablePiece p) {
      ed = p.getEditor();
      setLayout(new MigLayout("fill", "[grow,fill]", "[align top]rel[align bottom]")); //NON-NLS

      final JPanel scrollPanel = new JPanel(new MigLayout("wrap 1", "[grow,fill]", "[top]rel[bottom]")); // NON-NLS
      final JScrollPane scroll = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      scrollPanel.add(ed.getControls(), "growx,aligny top,wrap"); //NON-NLS
      add(scroll, "growx,wrap"); // NON-NLS

      final JPanel buttonBox = new JPanel(new MigLayout("ins 0", "push[]rel[]rel[]push")); // NON-NLS

      JButton b = new JButton(Resources.getString("General.ok"));
      b.addActionListener(evt -> dispose());

      buttonBox.add(b, "sg,tag ok"); //NON-NLS

      b = new JButton(Resources.getString("General.cancel"));
      b.addActionListener(evt -> {
        ed = null;
        dispose();
      });

      buttonBox.add(b, "sg,tag cancel"); //NON-NLS

      if (p.getHelpFile() != null) {
        b = new JButton(Resources.getString("General.help"));
        b.addActionListener(evt -> BrowserSupport.openURL(p.getHelpFile().getContents().toString()));
        buttonBox.add(b, "sg,tag help"); //NON-NLS
      }

      add(buttonBox, "center"); // NON-NLS-
      pack();
      setLocationRelativeTo(getOwner());
      SwingUtils.ensureOnScreen(this);
    }

    public PieceEditor getEditor() {
      return ed;
    }
  }

  protected void removeTrait(int index) {
    inUseModel.removeElementAt(index);
    if (index < inUseModel.size()) {
      ((Decorator) inUseModel.elementAt(index)).setInner(inUseModel.elementAt(index - 1));
    }
    refresh();
    setChanged(true);
  }

  protected void addTrait(Decorator c) {
    addTrait(c, inUseList.getSelectedIndex());
  }

  protected void addTrait(Decorator c, int insertIndex) {
    final Class<? extends Decorator> cClass = c.getClass();
    Decorator d = null;
    try {
      d = cClass.getConstructor().newInstance();
    }
    catch (Throwable t) {
      ReflectionUtils.handleNewInstanceFailure(t, cClass);
    }

    if (d != null) {
      if (d instanceof PlaceMarker) {
        ((PlaceMarker) d).updateGpId(gpidSupport);
      }
      d.setInner(inUseModel.lastElement());
      inUseModel.addElement(d);
      moveDecorator(inUseModel.size() - 1, insertIndex == -1 ? inUseModel.size() : insertIndex + 1);
      setChanged(true);
    }

    refresh();
  }

  private static class Renderer extends DefaultListCellRenderer {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent(
      JList list, Object value, int index, boolean selected, boolean hasFocus) {

      // DO NOT pass value to super.getListCellRendererComponent()
      // It is incredibly inefficient for GamePieces and is not needed
      // since we overwrite the label text anyway.
      super.getListCellRendererComponent(list, "", index, selected, hasFocus);
      if (value instanceof EditablePiece) {
        setText(((EditablePiece) value).getDescription());
      }
      else {
        final String s = value.getClass().getName();
        setText(s.substring(s.lastIndexOf('.') + 1));
      }
      return this;
    }
  }

  /**
   * Contents of the Copy/Paste buffer for traits in the editor
   * @author rkinney
   *
   */
  private static class TraitClipboard {
    private final String type;
    private final String state;
    public TraitClipboard(Decorator copy) {
      type = copy.myGetType();
      state = copy.myGetState();
    }
    public String getType() {
      return type;
    }
    public String getState() {
      return state;
    }
  }

  static class ListTransferHandler extends TransferHandler {

    private static final long serialVersionUID = 1;

    private final PieceDefiner definer;
    private int fromIndex;
    private String fromList;


    public ListTransferHandler(PieceDefiner definer) {
      super();
      this.definer = definer;
    }

    public PieceDefiner getDefiner() {
      return definer;
    }

    public int getFromIndex() {
      return fromIndex;
    }

    public void setFromIndex(int fromIndex) {
      this.fromIndex = fromIndex;
    }

    public String getFromList() {
      return fromList;
    }

    public void setFromList(String fromList) {
      this.fromList = fromList;
    }

    /**
     * Called to see if we can drop our package at this point.
     */
    @Override
    public boolean canImport(TransferHandler.TransferSupport info) {
      // No Drop allowed in Available list
      if (AVAILABLE.equals((info.getComponent()).getName())) {
        return false;
      }
      else {
        // No drop allowed above BasicPiece in InUseList
        final JList<GamePiece> list = (JList<GamePiece>) info.getComponent();

        if (list.getModel().getSize() > 0 && list.getModel().getElementAt(0).getClass() == BasicPiece.class && info.getDropLocation().getDropPoint().y < 10) {
          return false;
        }
      }
      return info.isDataFlavorSupported(DataFlavor.stringFlavor);
    }

    /**
     * Called when the drag is started on the selected trait
     */
    @Override
    protected Transferable createTransferable(JComponent c) {
      final JList<GamePiece> list = (JList<GamePiece>) c;
      setFromIndex(list.getSelectedIndex());
      setFromList(list.getName());

      // Set a DragImage showing the trait description on systems that support it (Windows)
      if (DragSource.isDragImageSupported()) {
        setDragImage(((EditablePiece) list.getModel().getElementAt(getFromIndex())).getDescription());
      }

      return new StringSelection("");
    }

    private void setDragImage(String description) {
      final Font DRAG_FONT = new Font(Font.DIALOG, Font.BOLD, 12);
      final JLabel label = new JLabel();
      label.setFont(DRAG_FONT);
      label.setText(description);
      final int w = label.getFontMetrics(DRAG_FONT).stringWidth(description) + 2;
      final int h = label.getFontMetrics(DRAG_FONT).getHeight() + 2;
      final BufferedImage img = new BufferedImage(w, h, BufferedImage.TYPE_4BYTE_ABGR);
      final Graphics2D g2 = img.createGraphics();
      g2.setColor(Color.white);
      g2.fillRect(0, 0, w, h);
      g2.setColor(Color.black);
      g2.setFont(DRAG_FONT);
      g2.drawString(description, 2, h - 5);
      g2.dispose();

      setDragImage(img);
      setDragImageOffset(new Point(0, h));
    }
    /**
     * Move Only
     */
    @Override
    public int getSourceActions(JComponent c) {

      // Dragging fails if nothing selected in source, should never happen
      if (getFromIndex() < 0) {
        return TransferHandler.NONE;
      }

      return TransferHandler.MOVE;
    }

    /**
     * Perform the actual import. Called when the drag is dropped.
     */
    @Override
    public boolean importData(TransferHandler.TransferSupport info) {
      if (!info.isDrop() || ! (info.getComponent() instanceof JList)) {
        return false;
      }

      // The index where user wants to drop
      final int toIndex = ((JList.DropLocation) info.getDropLocation()).getIndex();

      if (toIndex > 0 && toIndex != getFromIndex()) {
        // If the source list is AVAILABLE, then we need to an Add
        if (AVAILABLE.equals(getFromList())) {
          getDefiner().doAdd(getFromIndex(), toIndex - 1);
        }
        else {
          // Ask the Definer to move the piece within the inUse list.
          getDefiner().moveDecorator(getFromIndex(), toIndex);
        }
      }

      return true;
    }
  }

  /**
   * KeyAdapter added to the InUseList. Look for cut/copy/paste and
   * call the matching buttons to do the work.
   */
  static class InUseListKeyAdapter extends KeyAdapter {

    private final PieceDefiner definer;

    public InUseListKeyAdapter(PieceDefiner definer) {
      this.definer = definer;
    }

    @Override
    public void keyReleased(KeyEvent e) {

      if (SwingUtils.isModifierKeyDown(e)) {
        switch (e.getKeyCode()) {
        case KeyEvent.VK_C:
          definer.copyButton.doClick();
          break;
        case KeyEvent.VK_V:
          definer.pasteButton.doClick();
          break;
        case KeyEvent.VK_X:
          definer.doCopy();
          definer.removeButton.doClick();
          break;
        default:
          break;
        }
      }
      else if (e.getKeyCode() == KeyEvent.VK_DELETE) {
        definer.removeButton.doClick();
      }
      else if (e.getKeyCode() == KeyEvent.VK_ENTER) {
        definer.propsButton.doClick();
      }
    }
  }

  /**
   * KeyAdapter added to the AvailableList. Look for Enter and
   * call the add button
   */
  static class AvailableListKeyAdapter extends KeyAdapter {

    private final PieceDefiner definer;

    public AvailableListKeyAdapter(PieceDefiner definer) {
      this.definer = definer;
    }

    @Override
    public void keyReleased(KeyEvent e) {
      if (e.getKeyCode() == KeyEvent.VK_ENTER) {
        definer.addButton.doClick();
      }
    }
  }

  /**
   * A PieceSlot that can be scaled externally, instead of relying
   * on the scale of an enclosing widget
   */
  static class ScaleablePieceSlot extends PieceSlot {
    private double scale = 1.0;

    public ScaleablePieceSlot() {
      super();
      setScale(1.0);
    }

    @Override
    public double getScale() {
      return scale;
    }

    public void setScale(double scale) {
      this.scale = scale;
    }
  }

}

