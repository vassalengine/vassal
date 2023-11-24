/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney, Brent Easton
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
package VASSAL.i18n;

import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.PropertiesWindow;
import VASSAL.configure.ShowHelpAction;
import VASSAL.tools.WarningDialog;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.swing.SwingUtils;

import org.apache.commons.lang3.SystemUtils;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.EventListenerList;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeSelectionModel;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.io.Serializable;
import java.util.EventObject;
import java.util.List;

/**
 * Window for editing translations of a {@link Configurable} object
 */
public class TranslateWindow extends JDialog implements ListSelectionListener,
    TreeSelectionListener {
  private static final long serialVersionUID = 1L;
  protected static final Color TRANSLATION_NEEDED_COLOR = Color.black;
  protected static final Color TRANSLATION_DONE_COLOR = Color.black;
  protected static final Color NO_TRANSLATION_NEEDED_COLOR = Color.black;

  protected Translatable target;

  protected String[] keys;
  protected JTable keyTable;
  protected Translatable keyTarget;
  protected JTree tree;
  protected Translation currentTranslation = null;
  protected JComboBox<String> langBox;
  protected ActionListener boxListener;
  protected int lastSelectedLangIndex;
  protected String currentKey = ""; //$NON-NLS-1$
  protected ConfigureTree myConfigureTree;
  protected CopyButton[] copyButtons;

  protected JButton okButton;
  protected JButton cancelButton;

  public TranslateWindow(Frame owner, boolean modal, final Translatable target, ConfigureTree tree) {
    super(owner, modal);
    this.target = target;
    myConfigureTree = tree;
    initComponents();
  }

  protected void initComponents() {

    // Default selection color on Mac makes Translation text unreadable. Change to same as Windows
    // NOTE: This is interim only until 3.8 when I will implement a Vassal-wide solution.
    if (SystemUtils.IS_OS_MAC) {
      UIManager.put("Table.selectionBackground", new Color(184, 207, 229));
      UIManager.put("Tree.selectionBackground", new Color(184, 207, 229));
    }

    setTitle(Resources.getString("Editor.TranslateWindow.translate", ConfigureTree.getConfigureName((Configurable) target)));
    final JPanel mainPanel = new JPanel(new BorderLayout());
    /*
     * Place Language selector above Tree and Keys
     */
    mainPanel.add(getHeaderPanel(), BorderLayout.PAGE_START);
    mainPanel.add(buildMainPanel(), BorderLayout.CENTER);
    mainPanel.add(getButtonPanel(), BorderLayout.PAGE_END);
    add(mainPanel);
    pack();

    // Default actions for Enter/ESC
    SwingUtils.setDefaultButtons(getRootPane(), okButton, cancelButton);

    setLocationRelativeTo(getParent());
    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent we) {
        cancel();
      }
    });
  }

  protected Component getHeaderPanel() {
    final JPanel langPanel = new JPanel();
    langPanel.add(new JLabel(Resources.getString("Editor.TranslateWindow.language")));
    langBox = new JComboBox<>(Localization.getInstance().getTranslationList());
    langPanel.add(langBox);
    boxListener = e -> {
      commitTableEdit();
      final String selectedTranslation = (String) ((JComboBox) e.getSource()).getSelectedItem();
      changeLanguage(selectedTranslation);
    };
    langBox.addActionListener(boxListener);
    if (Localization.getInstance().getTranslationList().length > 0) {
      langBox.setSelectedIndex(0);
    }
    langPanel.setMinimumSize(new Dimension(800, 0));

    final JButton addButton = new JButton(Resources.getString("Editor.TranslateWindow.add_translation"));
    addButton.addActionListener(e -> getNewTranslation());

    langPanel.add(addButton);

    return langPanel;
  }

  /**
   * User has clicked on the Add Translation button. Create a new
   * PropertiesWindow for a translation and display it.
   *
   */
  protected void getNewTranslation() {
    final Translation t = new Translation();
    final PropertiesWindow w = new MyPropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, this), false, t, null, this);
    w.setVisible(true);
  }

  /**
   * Called from MyPropertiesWindow when the user saves the new translation
   * @param target new Translation
   */
  protected void refreshTranslationList(Configurable target) {
    final Translation newTranslation = (Translation) target;

    final List<Language> list = GameModule.getGameModule().getComponentsOf(Language.class);
    if (list != null) {
      if (list.get(0).contains(newTranslation)) {
        WarningDialog.show("Editor.TranslateWindow.translation_exists", newTranslation.getDescription());
        return;
      }
      final Language language = list.iterator().next();
      myConfigureTree.externalInsert(language, target);
    }
    langBox.removeAllItems();
    final String[] langs = Localization.getInstance().getTranslationList();
    for (final String lang : langs) {
      langBox.addItem(lang);
    }
    langBox.setSelectedItem(newTranslation.getDescription());
    keyTable.setEnabled(true);
    tree.repaint();
  }

  protected static class MyPropertiesWindow extends PropertiesWindow {
    private static final long serialVersionUID = 1L;
    protected Configurable myTarget;
    protected TranslateWindow owningWindow;
    public MyPropertiesWindow(Frame owner, boolean modal, final Configurable target, HelpWindow helpWindow, TranslateWindow tw) {
      super(owner, modal, target, helpWindow);
      myTarget = target;
      owningWindow = tw;
    }
    @Override
    public void save() {
      super.save();
      owningWindow.refreshTranslationList(myTarget);
    }
    @Override
    public void cancel() {
      dispose();
    }
  }

  protected Component buildMainPanel() {
    final JPanel keyPanel = buildKeyTablePanel();

    /*
     * Tree of all components from target component down
     */
    final JPanel treePanel = new JPanel(new BorderLayout());
    final MyTreeNode top = new MyTreeNode(target);
    createNodes(top);
    tree = new JTree(top);
    tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    tree.addTreeSelectionListener(this);
    tree.setSelectionRow(0);
    tree.setCellRenderer(new MyTreeCellRenderer());
    final JScrollPane treeScroll = new JScrollPane(tree, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    treePanel.add(treeScroll, BorderLayout.CENTER);
    treePanel.setMinimumSize(new Dimension(400, 100));
    treePanel.setPreferredSize(new Dimension(800, 300));

    /*
     * First split between Tree display and Keys
     */
    final JSplitPane split1 = new JSplitPane(JSplitPane.VERTICAL_SPLIT, treePanel, keyPanel);
    split1.setResizeWeight(0.5);

    return split1;
  }

  protected JPanel buildKeyTablePanel() {
    /*
     * Key Panel - Table of Keys for the component currently selected in the
     * Tree Panel
     */
    final JPanel keyPanel = new JPanel(new BorderLayout());
    keyPanel.setMinimumSize(new Dimension(800, 100));
    keyTable = new MyTable();
    keyTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

    keyTable.addFocusListener(new FocusListener() {
      @Override
      public void focusGained(java.awt.event.FocusEvent e) {
      }
      @Override
      public void focusLost(java.awt.event.FocusEvent e) {
        commitTableEdit();
      }
    });

    keyTable.getSelectionModel().addListSelectionListener(this);
    keyTable.setEnabled(currentTranslation != null);

    final JScrollPane keyScroll = new JScrollPane(keyTable, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    keyPanel.add(keyScroll, BorderLayout.CENTER);
    keyPanel.setMinimumSize(new Dimension(400, 100));
    keyPanel.setPreferredSize(new Dimension(800, 200));
    return keyPanel;
  }

  protected Component getButtonPanel() {
    final JPanel buttonBox = new JPanel();

    final JButton helpButton = new JButton(Resources.getString(Resources.HELP));
    helpButton.addActionListener(new ShowHelpAction(HelpFile.getReferenceManualPage("Translations.html", "module").getContents(), null)); //NON-NLS
    buttonBox.add(helpButton);

    okButton = new JButton(Resources.getString(Resources.OK));
    okButton.addActionListener(e -> {
      try {
        save();
      }
      catch (IOException e1) {
        WriteErrorDialog.error(e1,
          GameModule.getGameModule().getArchiveWriter().getName());
      }
    });
    buttonBox.add(okButton);

    cancelButton = new JButton(
      Resources.getString(Resources.CANCEL));
    cancelButton.addActionListener(e -> cancel());
    buttonBox.add(cancelButton);

    return buttonBox;
  }

  // Workaround for JRE Bug 4709394 - Cell editing lost when JTable loses
  // focus. Call this all over the place!
  protected void commitTableEdit() {
    if (keyTable != null && keyTable.isEditing()) {
      final int row = keyTable.getEditingRow();
      final int column = keyTable.getEditingColumn();
      if (row != -1 && column != -1)
        keyTable.editCellAt(row, column);
    }
  }

  /**
   * New Language selected from the drop-down box
   */
  protected void changeLanguage(String selectedTranslation) {
    if (currentTranslation != null) {
      if (currentTranslation.isDirty()) {
        try {
          if (!querySave()) {
            langBox.removeActionListener(boxListener);
            langBox.setSelectedItem(lastSelectedLangIndex);
            langBox.addActionListener(boxListener);
          }
        }
        catch (IOException e) {
          WriteErrorDialog.error(e,
            GameModule.getGameModule().getArchiveWriter().getName());
        }
      }
    }
    currentTranslation = Localization.getInstance().getTranslation(selectedTranslation);
    lastSelectedLangIndex = langBox.getSelectedIndex();
    if (keyTable != null) {
      ((MyTableModel) keyTable.getModel()).update();
    }
  }

  /**
   * When a new node is selected, display keys for the new component in the keys
   * table
   */
  @Override
  public void valueChanged(TreeSelectionEvent e) {
    commitTableEdit();
    final MyTreeNode node = (MyTreeNode) tree.getLastSelectedPathComponent();
    if (node == null)
      return;
    keys = node.getTarget().getI18nData().getAttributeKeys().toArray(new String[0]);
    copyButtons = new CopyButton[keys.length];
    keyTarget = node.getTarget();
    ((AbstractTableModel) keyTable.getModel()).fireTableStructureChanged();
    if (keys != null && keys.length > 0) {
      keyTable.getSelectionModel().setSelectionInterval(0, 0);
    }
    ((MyTableModel) keyTable.getModel()).update();
  }


  /**
   * When a key is selected in the table, display the source and translated
   * texts in the right hand panels
   */
  @Override
  public void valueChanged(ListSelectionEvent e) {
    if (e.getValueIsAdjusting())
      return;

    final ListSelectionModel lsm = (ListSelectionModel) e.getSource();
    if (!lsm.isSelectionEmpty()) {
      final String key = keys[lsm.getMinSelectionIndex()];
      currentKey = keyTarget.getI18nData().getFullPrefix() + key; //$NON-NLS-1$
    }
  }

  /**
   * Create the nodes for the JTree display
   */

  protected void createNodes(MyTreeNode top) {
    for (final Translatable child : top.getTarget().getI18nData().getChildren()) {
      final MyTreeNode childNode = new MyTreeNode(child);
      createNodes(childNode);
      top.add(childNode);
    }
  }

  public static String getDisplayName(Translatable t) {
    if (t == null) {
      return ""; //$NON-NLS-1$
    }
    final String type = ConfigureTree.getConfigureName(t.getClass());
    String name = ""; //$NON-NLS-1$
    if (t instanceof Configurable) {
      name = ((Configurable) t).getConfigureName();
    }
    final String s = (name == null) ? "" : (name + " "); //$NON-NLS-1$ //$NON-NLS-2$
    return s + " [" + type + "]"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * Cancel button clicked. Check for outstanding changes.
   */
  protected void cancel() {
    commitTableEdit();
    if (currentTranslation != null) {
      if (currentTranslation.isDirty()) {
        try {
          if (!querySave()) {
            return;
          }
        }
        catch (IOException e) {
          WriteErrorDialog.error(e,
            GameModule.getGameModule().getArchiveWriter().getName());
        }
      }
    }
    dispose();
  }

  protected boolean querySave() throws IOException {
    switch (JOptionPane.showConfirmDialog(this, Resources.getString("Editor.TranslateWindow.save_now"),
      Resources.getString("Editor.TranslateWindow.unsaved"), JOptionPane.YES_NO_CANCEL_OPTION)) {
    case JOptionPane.YES_OPTION:
      saveTranslation();
      return true;
    case JOptionPane.NO_OPTION:
      reloadTranslation();
      return true;
    case JOptionPane.CANCEL_OPTION:
      return false;
    }
    return true;
  }

  /**
   * Save button clicked
   * @throws IOException oops
   */
  protected void save() throws IOException {
    commitTableEdit();
    if (saveTranslation()) {
      dispose();
    }
  }

  /**
   * Save the current Translation
   * @throws IOException oops
   */
  protected boolean saveTranslation() throws IOException {
    if (currentTranslation != null) {
      currentTranslation.saveProperties();
    }
    return true;
  }

  /**
   * Reload the current translation from the archive
   * @throws IOException oops
   */
  protected void reloadTranslation() throws IOException {
    if (currentTranslation != null) {
      currentTranslation.reloadProperties();
    }
  }

  /**
   * Custom JTable to support CopyButtons in JTable cells
   *
   */
  class MyTable extends JTable {

    private static final long serialVersionUID = 1L;

    public MyTable() {
      super(new MyTableModel());
      setDefaultRenderer(JComponent.class, new JComponentCellRenderer());
      setDefaultEditor(JComponent.class, new JComponentCellEditor());
    }

    @Override
    public TableCellRenderer getCellRenderer(int row, int column) {
      final TableColumn tableColumn = getColumnModel().getColumn(column);
      TableCellRenderer renderer = tableColumn.getCellRenderer();
      if (renderer == null) {
        Class<?> c = getColumnClass(column);
        if (c.equals(Object.class)) {
          final Object o = getValueAt(row, column);
          if (o != null)
            c = getValueAt(row, column).getClass();
        }
        renderer = getDefaultRenderer(c);
      }
      return renderer;
    }

    @Override
    public TableCellEditor getCellEditor(int row, int column) {
      final TableColumn tableColumn = getColumnModel().getColumn(column);
      TableCellEditor editor = tableColumn.getCellEditor();
      if (editor == null) {
        Class<?> c = getColumnClass(column);
        if (c.equals(Object.class)) {
          final Object o = getValueAt(row, column);
          if (o != null)
            c = getValueAt(row, column).getClass();
        }
        editor = getDefaultEditor(c);
      }
      return editor;
    }

  }

  /**
   * Custom Cell Renderer to support CopyButtons in JTable cells
   *
   */
  protected static class JComponentCellRenderer implements TableCellRenderer {
    @Override
    public Component getTableCellRendererComponent(JTable table, Object value,
                                                   boolean isSelected, boolean hasFocus, int row, int column) {
      return (JComponent)value;
    }
  }

  /**
   * Custom CellEditor to support CopyButtons in JTable cells
   *
   */
  protected static class JComponentCellEditor
                     implements TableCellEditor, TreeCellEditor, Serializable {

    private static final long serialVersionUID = 1L;

    protected EventListenerList listenerList = new EventListenerList();
    protected transient ChangeEvent changeEvent = null;

    protected JComponent editorComponent = null;
    protected JComponent container = null;    // Can be tree or table


    public Component getComponent() {
      return editorComponent;
    }


    @Override
    public Object getCellEditorValue() {
      return editorComponent;
    }

    @Override
    public boolean isCellEditable(EventObject anEvent) {
      return true;
    }

    @Override
    public boolean shouldSelectCell(EventObject anEvent) {
      if (editorComponent != null && anEvent instanceof MouseEvent
        && ((MouseEvent)anEvent).getID() == MouseEvent.MOUSE_PRESSED) {

        final Component dispatchComponent = SwingUtilities.getDeepestComponentAt(editorComponent, 3, 3);
        ((CopyButton) dispatchComponent).setSelected(true);
      }
      return false;
    }

    @Override
    public boolean stopCellEditing() {
      return true;
    }

    @Override
    public void cancelCellEditing() {
    }

    @Override
    public void addCellEditorListener(CellEditorListener l) {
    }

    @Override
    public void removeCellEditorListener(CellEditorListener l) {
    }

    // implements javax.swing.tree.TreeCellEditor
    @Override
    public Component getTreeCellEditorComponent(JTree tree, Object value,
      boolean isSelected, boolean expanded, boolean leaf, int row) {
//      String stringValue = tree.convertValueToText(value, isSelected,
//        expanded, leaf, row, false);

      editorComponent = (JComponent)value;
      container = tree;
      return editorComponent;
    }

    // implements javax.swing.table.TableCellEditor
    @Override
    public Component getTableCellEditorComponent(JTable table, Object value,
      boolean isSelected, int row, int column) {

      editorComponent = (JComponent)value;
      container = table;
      return editorComponent;
    }

  } // End of class JComponentCellEditor


  /**
   * Custom Key Table Model
   */
  static final int ATTR_COL = 0;
  static final int SOURCE_COL = 1;
  static final int CC_COL = 2;
  static final int TRAN_COL = 3;

  class MyTableModel extends AbstractTableModel {

    private static final long serialVersionUID = 1L;

    @Override
    public int getColumnCount() {
      return 4;
    }

    @Override
    public String getColumnName(int col) {
      switch (col) {
      case ATTR_COL:
        return Resources.getString("Editor.TranslateWindow.attribute");
      case SOURCE_COL:
        return Resources.getString("Editor.TranslateWindow.source_text");
      case CC_COL:
        return Resources.getString("Editor.TranslateWindow.cc");
      case TRAN_COL:
        return Resources.getString("Editor.TranslateWindow.translation");
      }
      return null;
    }

    @Override
    public int getRowCount() {
      return keys == null ? 0 : keys.length;
    }

    @Override
    public Object getValueAt(int row, int col) {
      switch (col) {
      case ATTR_COL:
        return keys == null ? null : keyTarget.getI18nData().getAttributeDescription(keys[row]);
      case SOURCE_COL:
        return keys == null ? null : keyTarget.getAttributeValueString(keys[row]);
      case CC_COL:
        if (copyButtons[row] == null) {
          copyButtons[row] = new CopyButton(row);
        }
        return copyButtons[row];

      case TRAN_COL:
        if (currentTranslation != null) {
          final String key = keyTarget.getI18nData().getFullPrefix() + keys[row]; //$NON-NLS-1$
          return currentTranslation.translate(key);
        }
      }
      return null;
    }

    @Override
    public void setValueAt(Object value, int row, int col) {
      if (col == TRAN_COL) {
        currentTranslation.setProperty(currentKey, (String) value);
        copyButtons[row].checkEnabled();
        fireTableCellUpdated(row, col);
        tree.repaint();
      }
    }

    @Override
    public boolean isCellEditable(int row, int col) {
      return col == TRAN_COL || col == CC_COL;
    }

    public void update() {
      this.fireTableStructureChanged();
      keyTable.getColumnModel().getColumn(ATTR_COL).setCellRenderer(new MyTableCellRenderer(keyTarget));
      keyTable.getColumnModel().getColumn(CC_COL).setMaxWidth(25);
    }

  }

  /**
   * Custom table cell renderer - Change color of key names based on translation
   * status
   */
  class MyTableCellRenderer extends DefaultTableCellRenderer {

    private static final long serialVersionUID = 1L;
    protected Translatable target;

    public MyTableCellRenderer(Translatable target) {
      this.target = target;
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
        boolean hasFocus, int row, int col) {
      final Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row,
          col);

      final String fullKey = target.getI18nData().getFullPrefix() + keys[row]; //$NON-NLS-1$
      final String translation = currentTranslation == null ? "" : currentTranslation.translate(fullKey);
      final String originalValue = target.getAttributeValueString(keys[row]);

      if (originalValue == null || originalValue.length() == 0) {
        c.setForeground(NO_TRANSLATION_NEEDED_COLOR);
        c.setFont(c.getFont().deriveFont(Font.PLAIN));
      }
      else {
        if (translation == null || translation.length() == 0) {
          c.setForeground(TRANSLATION_NEEDED_COLOR);
          c.setFont(c.getFont().deriveFont(Font.BOLD));
        }
        else {
          c.setForeground(TRANSLATION_DONE_COLOR);
          c.setFont(c.getFont().deriveFont(Font.PLAIN));
        }
      }

      return c;
    }
  }

  /**
   * Custom button to copy source to translation
   *
   */
  class CopyButton extends JButton implements ActionListener {
    private static final long serialVersionUID = 1L;
    int row;
    public CopyButton(int i) {
      super("->");
      row = i;
      addActionListener(this);
      setMargin(new Insets(1, 1, 1, 1));
      checkEnabled();
    }
    @Override
    public void actionPerformed(ActionEvent e) {
      final String key = keys[row];
      currentKey = keyTarget.getI18nData().getFullPrefix() +  key; //$NON-NLS-1$
      currentTranslation.setProperty(currentKey, keyTarget.getAttributeValueString(keys[row]));
      checkEnabled();
      ((MyTableModel) keyTable.getModel()).update();
    }
    public void checkEnabled() {
      if (keyTarget != null && keys != null && keys[row] != null) {
        final String t = currentTranslation == null ? "" : currentTranslation.translate(keyTarget.getI18nData().getFullPrefix() + keys[row]); //$NON-NLS-1$
        setEnabled(t == null || t.length() == 0);
      }
      else {
        setEnabled(true);
      }

    }
  }
  /**
   * Custom tree cell renderer - Change color of component names based on
   * translation status of children
   */
  class MyTreeCellRenderer extends DefaultTreeCellRenderer {

    private static final long serialVersionUID = 1L;

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel,
        boolean expanded, boolean leaf, int row, boolean hasFocus) {

      final Component c = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row,
          hasFocus);

      final Translatable t = ((MyTreeNode) value).getTarget();
      if (t.getI18nData().hasUntranslatedAttributes(currentTranslation)) {
        c.setForeground(TRANSLATION_NEEDED_COLOR);
        c.setFont(c.getFont().deriveFont(Font.BOLD));
      }
      else {
        c.setForeground(NO_TRANSLATION_NEEDED_COLOR);
        c.setFont(c.getFont().deriveFont(Font.PLAIN));
      }
      return c;
    }

  }

  /**
   * Custom Tree Node implementation
   */
  protected static class MyTreeNode extends DefaultMutableTreeNode {
    private static final long serialVersionUID = 1L;
    Translatable component;

    public MyTreeNode(Translatable t) {
      component = t;
    }

    public Translatable getTarget() {
      return component;
    }

    @Override
    public String toString() {
      return getDisplayName(component);
    }
  }
}
