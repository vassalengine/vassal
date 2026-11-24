package VASSAL.configure;

import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.util.Collections;
import java.util.List;
import javax.swing.AbstractListModel;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * The {@link Configurer} for {@link #ZOOM_LEVELS} and {@link #ZOOM_START}.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class LevelConfigurer extends Configurer {
  private final JPanel panel;
  private final LevelModel model;
  private final JList<String> levelList;
  private final JButton addButton;
  private final JButton removeButton;
  private final JButton initialButton;
  private final JTextField levelField;

  private final Levels levels;

  public LevelConfigurer(Levels l, String key, String name) {
    super(key, name);

    levels = l;

    panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));

    final Box leftBox = Box.createVerticalBox();
    final Box addBox = Box.createHorizontalBox();

    // Add button
    addButton = new JButton(Resources.getString(Resources.ADD));
    addButton.addActionListener(e -> addLevel());

    addButton.setEnabled(false);
    addBox.add(addButton);

    levelField = new JTextField(8);
    levelField.setMaximumSize(new Dimension(
      Integer.MAX_VALUE, levelField.getPreferredSize().height));

    // Edit box selects all text when first focused
    levelField.addFocusListener(new FocusAdapter() {
      @Override
      public void focusGained(FocusEvent evt) {
        SwingUtilities.invokeLater(new Runnable() {
          @Override
          public void run() {
            levelField.selectAll();
          }
        });
      }
    });

    // validator for the level entry field
    levelField.getDocument().addDocumentListener(new DocumentListener() {
      @Override
      public void changedUpdate(DocumentEvent e) { }

      @Override
      public void insertUpdate(DocumentEvent e) {
        validate();
      }

      @Override
      public void removeUpdate(DocumentEvent e) {
        validate();
      }

      private static final String PATTERN =
        "^(\\d*[1-9]\\d*(/\\d*[1-9]\\d*|\\.\\d*)?|0*\\.\\d*[1-9]\\d*)$"; //$NON-NLS-1$

      private void validate() {
        // valid entries match the pattern and aren't already in the list
        final String text = levelField.getText();
        addButton.setEnabled(text.matches(PATTERN) &&
          !levels.getLevels().contains(parseLevel(text)));
      }
    });

    // rely on addButton to do the validation
    levelField.addActionListener(e -> {
      if (addButton.isEnabled()) addLevel();
    });

    addBox.add(levelField);

    leftBox.add(addBox);

    final Box buttonBox = Box.createHorizontalBox();

    // Remove button
    removeButton = new JButton(Resources.getString(Resources.REMOVE));
    removeButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        // get the zoom level index to be removed
        final int rm_level = levelList.getSelectedIndex();
        final List<Double> l = levels.getLevels();

        final int new_init;
        if (rm_level == levels.getInitialLevel()) {
          // we're deleting the initial level; keep it the same position
          new_init = Math.min(rm_level, levels.getLevelCount() - 2);
          l.remove(rm_level);
        }
        else {
          // find the new index of the old initial level
          final Double old_init_val = l.get(levels.getInitialLevel());
          l.remove(rm_level);
          new_init = l.indexOf(old_init_val);
        }

        // adjust the state
        levels.reset(l, new_init);
        model.updateModel();

        // adjust the selection
        levelList.setSelectedIndex(
          Math.max(Math.min(rm_level, l.size() - 1), 0));
        updateButtons();
      }
    });

    buttonBox.add(removeButton);

    // Set Initial button
    initialButton = new JButton(Resources.getString("Editor.zoom.set_initial")); //$NON-NLS-1$
    initialButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        // set the new initial scale level
        final int i = levelList.getSelectedIndex();
        levels.reset(levels.getLevels(), i);
        model.updateModel();
        updateButtons();
      }
    });

    buttonBox.add(initialButton);

    leftBox.add(buttonBox);

    final JLabel explanation =
      new JLabel(Resources.getString("Editor.zoom.initial_zoom")); //$NON-NLS-1$
    explanation.setAlignmentX(JLabel.CENTER_ALIGNMENT);

    leftBox.add(
      Box.createVerticalStrut(explanation.getPreferredSize().height));
    leftBox.add(explanation);
    leftBox.add(
      Box.createVerticalStrut(explanation.getPreferredSize().height));

    // level list
    model = new LevelModel();
    levelList = new JList<>(model);
    levelList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    levelList.setSelectedIndex(0);

    levelList.addListSelectionListener(e -> updateButtons());

    final JSplitPane pane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    pane.setLeftComponent(leftBox);
    pane.setRightComponent(new JScrollPane(levelList));

    panel.add(pane);
    panel.setBorder(new TitledBorder(name));
    updateButtons();
  }

  /**
   * Parse a <code>String</code> to a <code>double</code>.
   * Accepts fractions as "n/d".
   */
  protected double parseLevel(String text) {
    final String[] s = text.split("/"); //$NON-NLS-1$
    try {
      return s.length > 1 ?
        Double.parseDouble(s[0]) / Double.parseDouble(s[1]) :
        Double.parseDouble(s[0]);
    }
    catch (final NumberFormatException ex) {
      // should not happen, text already validated
      ErrorDialog.bug(ex);
    }
    return 0.0;
  }

  /**
   * Add a level to the level list. This method expects that the
   * input has already been validated.
   */
  protected void addLevel() {
    // get the initial scale level
    final List<Double> l = levels.getLevels();
    final Double old_init_val = l.get(levels.getInitialLevel());

    // add the new scale level
    final double new_level_val = parseLevel(levelField.getText());
    l.add(new_level_val);
    Collections.sort(l);

    // find the initial scale index
    final int new_init = l.indexOf(old_init_val);

    // adjust the state
    levels.reset(l, new_init);
    model.updateModel();

    // adjust the selection
    final int new_level = l.indexOf(new_level_val);
    levelList.setSelectedIndex(new_level);

    levelField.setText("");
    updateButtons();
  }

  /**
   * Ensures that the buttons are properly en- or disabled.
   */
  protected void updateButtons() {
    removeButton.setEnabled(levels.getLevelCount() > 1);
    initialButton.setEnabled(
      levelList.getSelectedIndex() != levels.getInitialLevel());
  }

  /**
   * A {@link ListModel} built on the {@link State}.
   */
  protected class LevelModel extends AbstractListModel<String> {
    private static final long serialVersionUID = 1L;

    public void updateModel() {
      fireContentsChanged(this, 0, levels.getLevelCount() - 1);
    }

    @Override
    public String getElementAt(int i) {
      return levels.getLevels().get(i) +
        (levels.getInitialLevel() == i ? " *" : "");
    }

    @Override
    public int getSize() {
      return levels.getLevelCount();
    }
  }

  @Override
  public Component getControls() {
    return panel;
  }

  @Override
  public void setValue(Object o) {
  }

  @Override
  public void setValue(String s) {
  }

  @Override
  public String getValueString() {
    return null;
  }
}
