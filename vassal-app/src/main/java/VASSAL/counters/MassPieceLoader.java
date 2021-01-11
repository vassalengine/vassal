/*
 *
 * Copyright (c) 2008-2012 by Brent Easton
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

import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.CardSlot;
import VASSAL.build.widget.PieceSlot;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.swing.Dialogs;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultCellEditor;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumnModel;

import net.miginfocom.swing.MigLayout;

import org.jdesktop.swingx.JXTreeTable;
import org.jdesktop.swingx.treetable.DefaultMutableTreeTableNode;
import org.jdesktop.swingx.treetable.DefaultTreeTableModel;

/**
 * Class to load a directory full of images and create counters
 *
 */
public class MassPieceLoader {

  protected static final int DESC_COL = 0;
  protected static final int NAME_COL = 2;
  protected static final int IMAGE_COL = 1;
  protected static final int SKIP_COL = 3;
  protected static final int COPIES_COL = 4;
  protected static final int COLUMN_COUNT = 5;
  protected static final Color EDITABLE_COLOR = Color.blue;

  protected Configurable target;
  protected ConfigureTree configureTree;
  private final List<String> imageNames = new ArrayList<>();
  private final List<String> baseImages = new ArrayList<>();
  private final List<String> levelImages = new ArrayList<>();
  private final Map<String, PieceInfo> pieceInfo = new HashMap<>();
  private final List<Emb> layers = new ArrayList<>();
  protected MassLoaderDialog dialog;
  private static final DirectoryConfigurer dirConfig = new DirectoryConfigurer(null, "");
  private static final BooleanConfigurer basicConfig = new BooleanConfigurer(null, "", Boolean.FALSE);
  private static final MassPieceDefiner definer = new MassPieceDefiner();

  public MassPieceLoader(ConfigureTree tree, Configurable target) {
    this.target = target;
    this.configureTree = tree;
  }

  // The Dialog does all the work.
  public void load() {
    dialog = new MassLoaderDialog();
    dialog.setVisible(true);
    if (!dialog.isCancelled()) {
      dialog.load();
    }
  }

  /**
   * Mass Piece Loader dialog
   */
  class MassLoaderDialog extends JDialog {
    private static final long serialVersionUID = 1L;
    protected boolean cancelled = false;
    protected DefineDialog defineDialog;
    protected MyTreeTable tree;
    protected MyTreeTableModel model;
    protected BasicNode root;
    protected File loadDirectory;

    public MassLoaderDialog() {
      super(configureTree.getFrame());
      setModal(true);
      setTitle(Resources.getString("Editor.MassPieceLoader.load_multiple"));
      setLayout(new MigLayout("ins panel,wrap 2," + TraitLayout.STANDARD_GAPY, "[right]rel[grow,fill]", "[][][][grow,fill][]")); // NON-NLS
      setPreferredSize(new Dimension(800, 600));

      dirConfig.addPropertyChangeListener(e -> {
        if (e.getNewValue() != null) {
          buildTree((File) e.getNewValue());
        }
      });
      add(new JLabel(Resources.getString("Editor.MassPieceLoader.image_directory")));
      add(dirConfig.getControls(), "grow"); // NON-NLS

      basicConfig.addPropertyChangeListener(e -> {
        if (e.getNewValue() != null) {
          buildTree(dirConfig.getFileValue());
        }
      });
      add(new JLabel(Resources.getString("Editor.MassPieceLoader.no_basic_piece")));
      add(basicConfig.getControls());

      defineDialog = new DefineDialog(this);
      final JButton defineButton = new JButton(Resources.getString("Editor.MassPieceLoader.edit_piece_template"));
      defineButton.addActionListener(e -> {
        final GamePiece savePiece = definer.getPiece();
        defineDialog.setVisible(true);
        if (defineDialog.isCancelled()) {
          definer.setPiece(savePiece);
        }
        else {
          buildTree(dirConfig.getFileValue());
        }
      });
      add(defineButton, "skip 1,growx 0"); // NON-NLS

      tree = new MyTreeTable();
      buildTree(dirConfig.getFileValue());
      //final JPanel treePanel = new JPanel(new MigLayout("ins 0", "[grow,fill]", "[grow,fill]")); // NON-NLS
      //treePanel.add(tree, "grow"); // NON-NLS
      final JScrollPane scrollPane = new JScrollPane(tree);
      add(scrollPane, "span 2,grow"); // NON-NLS

      final JPanel buttonBox = new JPanel(new MigLayout("ins 0", "push[]rel[]rel[]push")); // NON-NLS
      final JButton okButton = new JButton(Resources.getString(Resources.OK));
      okButton.addActionListener(e -> save());
      buttonBox.add(okButton);

      final JButton cancelButton = new JButton(Resources
          .getString(Resources.CANCEL));
      cancelButton.addActionListener(e -> cancel());
      buttonBox.add(cancelButton);

      final JButton helpButton = new JButton(Resources
          .getString(Resources.HELP));
      helpButton.addActionListener(e -> {
        final HelpFile h = HelpFile.getReferenceManualPage("MassPieceLoader.html"); //NON-NLS
        BrowserSupport.openURL(h.getContents().toString());
      });
      buttonBox.add(helpButton);

      add(buttonBox, "span 2,center,grow 0"); // NON-NLS

      pack();
      setLocationRelativeTo(getParent());
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent we) {
          cancel();
        }
      });
    }

    public void cancel() {
      cancelled = true;
      dispose();
    }

    public void save() {
      // Count the pieces to be loaded
      int pieceCount = 0;
      for (int i = 0; i < root.getChildCount(); i++) {
        final PieceNode node = (PieceNode) root.getChildAt(i);
        if (! node.isSkip()) {
          pieceCount += node.getCopies();
        }
      }

      // Do they really want to do this?
      if (pieceCount > 0) {
        final String message = Resources.getString("Editor.MassPieceLoader.this_will_load", pieceCount, target.getConfigureName());
        final int result = Dialogs.showConfirmDialog(null, Resources.getString("Editor.MassPieceLoader.confirm_load"),
          Resources.getString("Editor.MassPieceLoader.confirm_load"), message, JOptionPane.WARNING_MESSAGE,
            JOptionPane.YES_NO_CANCEL_OPTION);
        if (result == 1) {
          cancel();
          return;
        }
        else if (result == 2) {
          return;
        }
      }

      cancelled = false;
      dispose();
    }

    public boolean isCancelled() {
      return cancelled;
    }

    public File getDirectory() {
      return dirConfig.getFileValue();
    }

    /**
     * Build a tree representing the Game Pieces, Layers, Levels and images to
     * be loaded. This tree is used as a model for the JxTreeTable, and also as
     * the guide to load the counters.
     *
     * @param dir
     *          Directory containing images
     */
    protected void buildTree(File dir) {

      loadDirectory = dir;

      // Make a list of the Layer traits in the template
      layers.clear();
      GamePiece piece = definer.getPiece();
      while (piece instanceof Decorator) {
        piece = Decorator.getDecorator(piece, Emb.class);
        if (piece instanceof Emb) {
          layers.add(0, (Emb) piece);
          piece = ((Emb) piece).getInner();
        }
      }

      // Find all of the images in the target directory
      loadImageNames(dir);

      // Check each image in the target directory to see if it matches the
      // level specification in any of the Embellishments in the template.
      // The remaining images that do not match any Level are our baseImages
      baseImages.clear();
      levelImages.clear();
      for (final String imageName : imageNames) {
        boolean match = false;
        for (final Emb emb : layers) {
          match = emb.matchLayer(imageName);
          if (match) {
            break;
          }
        }

        if (match) {
          levelImages.add(imageName);
        }
        else {
          baseImages.add(imageName);
        }
      }

      // Generate a table node for each base Image.
      // Create a child Layer Node for each layer that has at least one image
      root = new BasicNode();
      for (final String baseImage : baseImages) {
        final BasicNode pieceNode = new PieceNode(baseImage);
        for (final Emb emb : layers) {
          final Emb newLayer = new Emb(emb.getType(), null);
          if (newLayer.buildLayers(baseImage, levelImages)) {
            final BasicNode layerNode = new LayerNode(newLayer.getLayerName());
            for (int i = 0; i < newLayer.getImageNames().length; i++) {
              final String levelName = newLayer.getLevelNames()[i];
              final BasicNode levelNode = new LevelNode(
                levelName == null ? "" : levelName, newLayer
                .getImageNames()[i], i);
              layerNode.add(levelNode);
            }
            pieceNode.add(layerNode);
          }
        }
        root.add(pieceNode);
      }

      // Set the tree
      model = new MyTreeTableModel(root);
      tree.setTreeTableModel(model);

      final TableColumnModel tcm = tree.getColumnModel();
      tcm.getColumn(DESC_COL).setPreferredWidth(100);
      tcm.getColumn(DESC_COL).setCellRenderer(new ImageNameRenderer());
      tcm.getColumn(NAME_COL).setPreferredWidth(200);
      tcm.getColumn(NAME_COL).setCellRenderer(new NameRenderer());
      tcm.getColumn(NAME_COL).setCellEditor(new NameEditor(new JTextField()));
      tcm.getColumn(IMAGE_COL).setPreferredWidth(200);
      tcm.getColumn(IMAGE_COL).setCellRenderer(new ImageNameRenderer());
      tcm.getColumn(SKIP_COL).setPreferredWidth(50);
      tcm.getColumn(SKIP_COL).setMaxWidth(50);
      tcm.getColumn(COPIES_COL).setPreferredWidth(50);
      tcm.getColumn(COPIES_COL).setMaxWidth(50);
      tcm.getColumn(COPIES_COL).setCellRenderer(new CopiesRenderer());
    }

    class NameRenderer extends DefaultTableCellRenderer {
      private static final long serialVersionUID = 1L;

      @Override
      public Component getTableCellRendererComponent(JTable table,
          Object value, boolean isSelected, boolean hasFocus, int row, int col) {
        final Component c = super.getTableCellRendererComponent(table, value,
            isSelected, hasFocus, row, col);
        final BasicNode node = (BasicNode) tree.getPathForRow(row)
            .getLastPathComponent();
        c.setEnabled(!node.isSkip());
        c.setForeground(EDITABLE_COLOR);
        return c;
      }
    }

    class CopiesRenderer extends DefaultTableCellRenderer {
      private static final long serialVersionUID = 1L;

      @Override
      public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
        final JLabel renderedLabel = (JLabel) super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        final BasicNode node = (BasicNode) tree.getPathForRow(row).getLastPathComponent();
        renderedLabel.setHorizontalAlignment(SwingConstants.CENTER);
        renderedLabel.setEnabled(!node.isSkip());
        renderedLabel.setForeground(EDITABLE_COLOR);
        return renderedLabel;
      }
    }

    class ImageNameRenderer extends DefaultTableCellRenderer {
      private static final long serialVersionUID = 1L;

      @Override
      public Component getTableCellRendererComponent(JTable table,
          Object value, boolean isSelected, boolean hasFocus, int row, int col) {
        final DefaultTableCellRenderer c = (DefaultTableCellRenderer) super.getTableCellRendererComponent(table, value,
            isSelected, hasFocus, row, col);
        final BasicNode node = (BasicNode) tree.getPathForRow(row)
            .getLastPathComponent();
        c.setEnabled(!node.isSkip());
        if (node instanceof PieceNode) {
          final String image = node.getImageName();
          final String i = "<html><img src=\"file:/" + loadDirectory.getAbsolutePath() + "/" + image + "\"></html>"; //NON-NLS
          c.setToolTipText(i);
        }
        return c;
      }
    }

    class NameEditor extends DefaultCellEditor {
      private static final long serialVersionUID = 1L;

      public NameEditor(JTextField textField) {
        super(textField);
      }

      @Override
      public Component getTableCellEditorComponent(JTable table, Object value,
          boolean isSelected, int row, int column) {
        final Component c = super.getTableCellEditorComponent(table, value, isSelected, row, column);
        c.setForeground(Color.blue);
        return c;
      }
    }

    /**
     * Load all image names in the target directory
     *
     * @param dir Image Directory
     */
    protected void loadImageNames(File dir) {
      imageNames.clear();
      if (dir != null && dir.isDirectory()) {
        final File[] files = dir.listFiles();
        if (files != null) {
          Arrays.sort(files);
          for (final File file : files) {
            final String imageName = file.getName();
            if (ImageUtils.hasImageSuffix(imageName)) {
              imageNames.add(imageName);
            }
          }
        }
      }
    }

    /**
     * Load the Pieces based on the Node Tree built while user was editing
     */
    public void load() {
      // Check a Directory has been entered
      final File dir = dialog.getDirectory();
      if (dir == null) {
        return;
      }

      // For each PieceNode, load the required images into the module and
      // generate the piece
      for (int i = 0; i < root.getChildCount(); i++) {
        final PieceNode pieceNode = (PieceNode) root.getChildAt(i);
        if (!pieceNode.isSkip()) {
          load(pieceNode);
        }
      }
    }

    /**
     * Load a specific piece and and all referenced images.
     *
     * @param pieceNode
     *          Sub-tree representing piece
     */
    public void load(PieceNode pieceNode) {

      // Add the Base Image to the module
      final String baseImage = pieceNode.getBaseImageName();
      addImageToModule(baseImage);

      // Create the BasicPiece
      final String basicType =
        new SequenceEncoder("", ';')
          .append("")
          .append(basicConfig.booleanValue() ? "" : baseImage)
          .append(pieceNode.getName()).getValue();

      final BasicPiece basic = (BasicPiece) GameModule.getGameModule().createPiece(BasicPiece.ID + basicType);

      // Build the piece from the template
      GamePiece template = definer.getPiece();
      final ArrayList<Decorator> traits = new ArrayList<>();

      // Reverse the order of the traits to innermost out
      while (template instanceof Decorator) {
        traits.add(0, (Decorator) template);
        template = ((Decorator) template).getInner();
      }

      for (int count = 0; count < pieceNode.getCopies(); count++) {
        GamePiece piece = basic;
        // Build the new piece. Note special Handling for Embellishment templates
        // that will
        // have actual images added for references to matching images. If an
        // Embellishment
        // has no matching images at all, do not add it to the new counter.
        for (final Decorator trait : traits) {
          if (trait instanceof Emb) {
            final Emb newLayer = new Emb(trait.myGetType(), null);
            if (newLayer.buildLayers(baseImage, levelImages)) {
              for (final String image : newLayer.getBuiltImageList()) {
                addImageToModule(image);
              }
              newLayer.setInner(piece);
              final String saveState = newLayer.getState();
              piece = GameModule.getGameModule().createPiece(newLayer.getType());
              piece.setState(saveState);
            }
          }
          else {
            final Decorator newTrait = (Decorator) GameModule.getGameModule().createPiece(trait.getType());
            newTrait.setState(trait.getState());
            newTrait.setInner(piece);
            final String saveState = newTrait.getState();
            piece = GameModule.getGameModule().createPiece(newTrait.getType());
            piece.setState(saveState);
          }
        }

        // Create the PieceSlot for the new piece
        PieceSlot slot = null;
        final Class<?>[] c = target.getAllowableConfigureComponents();
        for (int i = 0; i < c.length && slot == null; i++) {
          if (c[i].equals(CardSlot.class)) {
            slot = new CardSlot();
            slot.setPiece(piece);
          }
        }
        if (slot == null) {
          slot = new PieceSlot(piece);
        }

        // Generate a gpid
        configureTree.updateGpIds(slot);

        // Add the new piece to the tree
        configureTree.externalInsert(target, slot);
      }
    }
  }

  /**
   * Add the named image to the module
   *
   * @param name
   *          Image name
   */
  protected void addImageToModule(String name) {
    if (name != null && name.length() > 0) {
      try {
        GameModule.getGameModule().getArchiveWriter().addImage(
            new File(dirConfig.getFileValue(), name).getCanonicalPath(), name);
      }
      catch (IOException e) {
        // FIXME: Log error properly
        // ErrorLog.log()
      }
    }
  }

  /**
   * Maintain a record of all names changed by the user for image basenames.
   * Default name is image name with image suffix stripped.
   *
   * @param baseName
   *          Image name
   * @return user modified name
   */
  protected String getPieceName(String baseName) {
    final PieceInfo info = pieceInfo.get(baseName);
    return info == null ? ImageUtils.stripImageSuffix(baseName) : info
        .getName();
  }

  /**
   *
   * A custom piece definer based on the Prototype piece definer
   *
   */
  static class MassPieceDefiner extends PrototypeDefinition.Config.Definer {
    private static final long serialVersionUID = 1L;

    protected static DefaultListModel<GamePiece> newModel;

    public MassPieceDefiner() {
      super(GameModule.getGameModule().getGpIdSupport());

      // Build a replacement model that uses a modified Embellishment trait
      // with a replacement ImagePicker.
      if (newModel == null) {
        newModel = new DefaultListModel<>();
        for (final Enumeration<?> e = availableModel.elements(); e.hasMoreElements();) {
          final Object o = e.nextElement();
          if (o instanceof Embellishment) {
            newModel.addElement(new MassPieceLoader.Emb());
          }
          else {
            newModel.addElement((GamePiece) o);
          }
        }
      }

      availableList.setModel(newModel);
      setPiece(null);

    }
  }

  /**
   *
   * Dialog to hold the PieceDefiner used to specify the multi-load piece
   * template.
   *
   */
  private static class DefineDialog extends JDialog {
    private static final long serialVersionUID = 1L;
    protected boolean cancelled = false;

    public DefineDialog(JDialog owner) {
      super(owner);
      setModal(true);
      setVisible(false);
      setTitle(Resources.getString("Editor.MassPieceLoader.multiple_piece_template"));
      setLocationRelativeTo(owner);
      setLayout(new MigLayout("ins panel,wrap 1," + TraitLayout.STANDARD_GAPY, "[grow,fill]", "[grow,fill][]")); // NON-NLS
      add(definer, "grow"); // NON-NLS

      final JButton saveButton = new JButton(Resources.getString(Resources.SAVE));
      saveButton.addActionListener(e -> save());

      final JButton canButton = new JButton(Resources.getString(Resources.CANCEL));
      canButton.addActionListener(e -> cancel());

      final JPanel bbox = new JPanel(new MigLayout("ins 0", "push[][]push")); // NON-NLS
      bbox.add(saveButton);
      bbox.add(canButton);

      add(bbox, "center"); // NON-NLS

      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent we) {
          cancel();
        }
      });

      pack();
    }

    protected void save() {
      cancelled = false;
      setVisible(false);
    }

    protected void cancel() {
      cancelled = true;
      setVisible(false);
    }

    public boolean isCancelled() {
      return cancelled;
    }

  }

  /**
   * *************************************************************************
   * Custom Tree table model:- - Return column count - Return column headings
   */
  private class MyTreeTableModel extends DefaultTreeTableModel {

    public MyTreeTableModel(BasicNode rootNode) {
      super(rootNode);
    }

    @Override
    public int getColumnCount() {
      return COLUMN_COUNT;
    }

    @Override
    public String getColumnName(int col) {
      switch (col) {
      case DESC_COL:
        return Resources.getString("Editor.MassPieceLoader.item");
      case NAME_COL:
        return Resources.getString("Editor.MassPieceLoader.piece_name");
      case IMAGE_COL:
        return Resources.getString("Editor.MassPieceLoader.image_name");
      case SKIP_COL:
        return Resources.getString("Editor.MassPieceLoader.skip");
      case COPIES_COL:
        return Resources.getString("Editor.MassPieceLoader.copies");
      }
      return "";
    }

    @Override
    public Class<?> getColumnClass(int column) {
      if (column == SKIP_COL) {
        return Boolean.class;
      }
      else if (column == COPIES_COL) {
        return Integer.class;
      }
      else {
        return String.class;
      }
    }

    @Override
    public boolean isCellEditable(Object node, int column) {
      if (node instanceof PieceNode) {
        if (column == NAME_COL || column == COPIES_COL) {
          return !((PieceNode) node).isSkip();
        }
        else
          return column == SKIP_COL;
      }
      return false;
    }

    @Override
    public Object getValueAt(Object node, int column) {
      return ((BasicNode) node).getValueAt(column);
    }

    @Override
    public void setValueAt(Object value, Object node, int column) {
      if (node instanceof PieceNode) {
        if (column == NAME_COL) {
          ((PieceNode) node).setName((String) value);
        }
        else if (column == SKIP_COL) {
          ((PieceNode) node).setSkip((Boolean) value);
        }
        else if (column == COPIES_COL) {
          int val = value == null ? 1 : (Integer) value;
          if (val < 1) val = 1;
          ((PieceNode) node).setCopies(val);
        }
      }
      else {
        super.setValueAt(value, node, column);
      }
    }
  }

  /**
   * Custom implementation of JXTreeTable Fix for bug on startup generating
   * illegal column numbers
   *
   */
  private class MyTreeTable extends JXTreeTable {

    private static final long serialVersionUID = 1L;

    public MyTreeTable() {
      super();
      setRootVisible(false);
    }

    // Hide the Skip checkbox on rows other than Piece rows
    @Override
    public TableCellRenderer getCellRenderer(int row, int column) {
      if (column == SKIP_COL || column == COPIES_COL) {
        if (this.getPathForRow(row) == null) {
          return new NullRenderer();
        }
        if (!(this.getPathForRow(row).getLastPathComponent() instanceof PieceNode)) {
          return new NullRenderer();
        }
      }
      return super.getCellRenderer(row, column);
    }
  }

  /**
   * Blank Cell Renderer
   */
  private static class NullRenderer implements TableCellRenderer {
    @Override
    public Component getTableCellRendererComponent(JTable arg0, Object arg1,
        boolean arg2, boolean arg3, int arg4, int arg5) {
      return new JLabel("");
    }
  }

  /**
   * *************************************************************************
   * Custom TreeTable Node
   */
  private static class BasicNode extends DefaultMutableTreeTableNode {
    protected String name;
    protected String imageName;
    protected boolean skip;
    protected int copies;

    public BasicNode() {
      this("", "");
    }

    public BasicNode(String name, String imageName) {
      this.name = name;
      this.imageName = imageName;
      this.skip = false;
      this.copies = 1;
    }

    public String getName() {
      return name;
    }

    public void setName(String name) {
      this.name = name;
    }

    public String getImageName() {
      return imageName;
    }

    public void setImageName(String imageName) {
      this.imageName = imageName;
    }

    public boolean isSkip() {
      return skip;
    }

    public void setSkip(boolean b) {
      skip = b;
    }

    public void setCopies(int i) {
      copies = i;
    }

    public int getCopies() {
      return copies;
    }

    public String getDescription() {
      return Resources.getString("Editor.MassPieceLoader.root");
    }

    @Override
    public Object getValueAt(int col) {
      switch (col) {
      case DESC_COL:
        return getDescription();
      case NAME_COL:
        return getName();
      case IMAGE_COL:
        return getImageName();
      case SKIP_COL:
        return isSkip();
      case COPIES_COL:
        return getCopies();
      }
      return "";
    }

  }

  /**
   * Node representing a GamePiece to be loaded. imageName is the name of the
   * Basic Piece image
   *
   */
  private class PieceNode extends BasicNode {

    public PieceNode(String imageName) {
      super();
      setImageName(imageName);
      final PieceInfo info = pieceInfo.get(imageName);
      if (info == null) {
        setName(ImageUtils.stripImageSuffix(imageName));
        setSkip(false);
      }
      else {
        setName(info.getName());
        setSkip(info.isSkip());
      }
    }

    public String getBaseImageName() {
      return super.getImageName();
    }


    @Override
    public String getImageName() {
      if (basicConfig.booleanValue()) {
        return "";
      }
      else {
        return super.getImageName();
      }
    }

    @Override
    public String getDescription() {
      return Resources.getString("Editor.MassPieceLoader.piece_desc");
    }

    @Override
    public void setName(String name) {
      super.setName(name);
      pieceInfo.put(getImageName(), new PieceInfo(name, isSkip()));
    }

    @Override
    public void setSkip(boolean skip) {
      super.setSkip(skip);
      pieceInfo.put(getImageName(), new PieceInfo(getName(), skip));
    }
  }

  /**
   * Node representing a Layer trait of a GamePiece
   */
  private static class LayerNode extends BasicNode {
    public LayerNode(String name) {
      super(name, "");
    }

    @Override
    public String getDescription() {
      return Resources.getString("Editor.MassPieceLoader.layer_desc") + (name.length() > 0 ? " [" + name + "]" : "");
    }

    @Override
    public String getName() {
      return "";
    }
  }

  /**
   * Node representing an individual Image Level within a Layer trait
   *
   */
  private static class LevelNode extends BasicNode {
    int levelNumber;

    public LevelNode(String name, String imageName, int level) {
      super(name, imageName);
      levelNumber = level;
    }

    @Override
    public String getDescription() {
      return Resources.getString("Editor.MassPieceLoader.level") + " " + (levelNumber + 1)
          + (name.length() > 0 ? " [" + name + "]" : "");
    }

    @Override
    public String getName() {
      return "";
    }
  }

  /**
   * Utility class to hold user changes about pieces - Updated piece name - Skip
   * load flag
   *
   */
  private static class PieceInfo {
    protected String name;
    protected boolean skip;

    public PieceInfo(String name, boolean skip) {
      this.name = name;
      this.skip = skip;
    }

    public String getName() {
      return name;
    }

    public void setName(String name) {
      this.name = name;
    }

    public boolean isSkip() {
      return skip;
    }

    public void setSkip(boolean b) {
      skip = b;
    }
  }

  /**
   * Subclass of Embellishment to allow us to directly manipulate its members
   */
  private static class Emb extends Embellishment {
    private final List<String> builtImages = new ArrayList<>();

    public Emb() {
      super();
    }

    public Emb(String type, GamePiece p) {
      super(type, p);
    }

    public String[] getImageNames() {
      return imageName;
    }

    public String[] getLevelNames() {
      return commonName;
    }

    // Return true if the specified file name matches the level
    // definition of any level in this layer
    public boolean matchLayer(String s) {
      for (final String levelName : imageName) {
        if (match(s.split("\\.")[0], levelName)) {
          return true;
        }
      }
      return false;
    }

    protected boolean match(String s, String levelName) {
      if (levelName != null && levelName.length() > 1) { // Check 1 to skip
                                                         // command char
        if (levelName.charAt(0) == BASE_IMAGE.charAt(0)) {
          return false;
        }
        else if (levelName.charAt(0) == ENDS_WITH.charAt(0)) {
          return s.endsWith(levelName.substring(1));
        }
        else if (levelName.charAt(0) == INCLUDES.charAt(0)) {
          return s.contains(levelName.substring(1));
        }
        else if (levelName.charAt(0) == EQUALS.charAt(0)) {
          return s.equals(levelName.substring(1));
        }
        else {
          try {
            return Pattern.matches(levelName.substring(1), s);
          }
          catch (Exception ex) { // Invalid pattern

          }
        }
      }
      return false;
    }

      /**
       * Set the actual layer images based on a base image and the layer image
       * template specification.
       *
       * @param baseImage
       *          base Image name
       * @param levelImages
       *          List of available level images
       * @return true if at least one layer built
       */
    public boolean buildLayers(String baseImage, List<String> levelImages) {
      final String base = baseImage.split("\\.")[0];
      int count = 0;
      builtImages.clear();

      for (int i = 0; i < imageName.length; i++) {
        final String imageTemplate = imageName[i];
        String thisImage = null;
        if (imageTemplate.charAt(0) == BASE_IMAGE.charAt(0)) {
          thisImage = baseImage;
        }
        else {
          for (final Iterator<String> it = levelImages.iterator(); it.hasNext()
              && thisImage == null;) {
            final String checkImage = it.next();
            final String checkImageBase = checkImage.split("\\.")[0];
            if (imageTemplate.charAt(0) == EQUALS.charAt(0)) {
              if (match(checkImageBase, imageTemplate)) {
                thisImage = checkImage;
              }
            }
            else {
              if (checkImage.startsWith(base)) {
                if (match(checkImageBase, imageTemplate)) {
                  thisImage = checkImage;
                }
              }
            }
          }
        }
        imageName[i] = thisImage;
        if (thisImage != null) {
          count++;
          builtImages.add(thisImage);
        }
      }
      return count > 0;
    }

    public List<String> getBuiltImageList() {
      return builtImages;
    }

    public static class LoaderEd extends Embellishment.Ed {
      public LoaderEd(Embellishment e) {
        super(e);
      }

      @Override
      protected MultiImagePicker getImagePicker() {
        return new LoaderImagePicker();
      }

    }

    @Override
    public PieceEditor getEditor() {
      return new LoaderEd(this);
    }

  }

  /**
   * Replacement class for the MultiImagePicker to specify image match strings
   */
  private static class LoaderImagePicker extends MultiImagePicker {
    private static final long serialVersionUID = 1L;

    @Override
    public void addEntry() {
      final String entry = Resources.getString("Editor.MassPieceLoader.image") + " " + (imageListElements.size() + 1);
      imageListElements.addElement(entry);
      final Entry pick = new Entry();
      multiPanel.add(entry, pick);
      imageList.setSelectedIndex(imageListElements.size() - 1);
      cl.show(multiPanel, imageList.getSelectedValue());
    }

    @Override
    public List<String> getImageNameList() {
      final int size = imageListElements.size();
      final ArrayList<String> names = new ArrayList<>(size);
      for (int i = 0; i < size; ++i) {
        names.add((multiPanel.getComponent(i).toString()));
      }
      return names;
    }

    @Override
    public void clear() {
      multiPanel.removeAll();
      imageListElements.removeAllElements();
    }

    @Override
    public void setImageList(String[] names) {
      while (names.length > multiPanel.getComponentCount()) {
        addEntry();
      }

      for (int i = 0; i < names.length; ++i) {
        if (names[i] != null) {
          ((Entry) multiPanel.getComponent(i)).setImageName(names[i]);
        }
      }
    }
  }

  protected static final String ENDS_WITH = "ends with"; //NON-NLS (really)
  protected static final String INCLUDES = "includes"; //NON-NLS (really)
  protected static final String MATCHES = "matches"; //NON-NLS (really)
  protected static final String EQUALS = "same as"; //NON-NLS (really)
  protected static final String BASE_IMAGE = "use Base Image"; //NON-NLS (really)

  private static class Entry extends JPanel {
    private static final long serialVersionUID = 1L;
    private final TranslatingStringEnumConfigurer typeConfig;
    private final StringConfigurer nameConfig;
    private final JLabel warning = new JLabel(Resources.getString("Editor.MassPieceLoader.warning_suffix"));

    public Entry() {
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
      add(new JLabel(
        Resources.getString("Editor.MassPieceLoader.do_not_suffix")));

      final Box entry = Box.createHorizontalBox();
      entry.add(new JLabel(Resources.getString("Editor.MassPieceLoader.image_name") + " "));
      typeConfig = new TranslatingStringEnumConfigurer(null, "",
        new String[] { ENDS_WITH, INCLUDES, MATCHES, EQUALS, BASE_IMAGE },
        new String[] {
          "Editor.MassPieceLoader.ends_with",
          "Editor.MassPieceLoader.includes",
          "Editor.MassPieceLoader.matches",
          "Editor.MassPieceLoader.same_as",
          "Editor.MassPieceLoader.base_image"
        }
        );
      entry.add(typeConfig.getControls());
      nameConfig = new StringConfigurer(null, "");
      entry.add(nameConfig.getControls());
      add(entry);

      warning.setVisible(false);
      add(warning);

      typeConfig.addPropertyChangeListener(e -> updateVisibility());

      nameConfig.addPropertyChangeListener(e -> updateVisibility());
    }

    protected void updateVisibility() {
      warning
          .setVisible(ImageUtils.hasImageSuffix(nameConfig.getValueString()));
      nameConfig.getControls().setVisible(
          !typeConfig.getValueString().equals(BASE_IMAGE));
    }

    @Override
    public String toString() {
      return typeConfig.getValueString().charAt(0)
          + nameConfig.getValueString();
    }

    public void setImageName(String s) {
      switch (s.charAt(0)) {
      case 'e':
        typeConfig.setValue(ENDS_WITH);
        break;
      case 'i':
        typeConfig.setValue(INCLUDES);
        break;
      case 'm':
        typeConfig.setValue(MATCHES);
        break;
      case 's':
        typeConfig.setValue(EQUALS);
        break;
      case 'u':
        typeConfig.setValue(BASE_IMAGE);
        break;
      }

      nameConfig.setValue(s.substring(1));
    }
  }
}
