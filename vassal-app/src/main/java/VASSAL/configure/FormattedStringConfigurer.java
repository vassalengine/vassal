package VASSAL.configure;

import VASSAL.i18n.Resources;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.SwingUtilities;
import javax.swing.text.JTextComponent;

public class FormattedStringConfigurer extends StringConfigurer implements ActionListener, FocusListener, MouseListener {
  private final DefaultComboBoxModel<String> optionsModel = new DefaultComboBoxModel<>();
  private JComboBox<String> dropList;
  private boolean processingSelection = false;
  private boolean textFieldFocusable = true;

  public FormattedStringConfigurer(String key, String name) {
    this(key, name, new String[0]);
  }

  public FormattedStringConfigurer(String[] options) {
    this(null, "", options);
  }

  public FormattedStringConfigurer(String key, String name, String[] options) {
    super(key, name);
    setOptions(options);
  }

  public void setOptions(String[] options) {
    optionsModel.removeAllElements();
    optionsModel.addElement(Resources.getString("Editor.FormattedStringConfigurer.insert"));
    for (final String option : options) {
      optionsModel.addElement(option);
    }
    setListVisibility();
  }

  public String[] getOptions() {
    final String[] s = new String[optionsModel.getSize() - 1];
    for (int i = 1; i < optionsModel.getSize(); ++i) {
      s[i - 1] = optionsModel.getElementAt(i);
    }
    return s;
  }

  @Override
  public Component getControls() {
    if (p == null) {
      super.getControls();

      dropList = new JComboBox<>(optionsModel) {
        @Override
        public void setPopupVisible(boolean visible) {
          super.setPopupVisible(visible);
          if(nameField != null){
            nameField.setFocusable(!visible);
            textFieldFocusable = !visible;
          }
        }
      };

      dropList.setSelectedIndex(0);
      dropList.setEnabled(false);
      dropList.addActionListener(this);
      dropList.addMouseListener(this);

      nameField.addFocusListener(this);
      setListVisibility();
      p.add(dropList, "grow 0,right");
    }
    return p;
  }

  private void setListVisibility() {
    if (dropList != null) {
      dropList.setVisible(optionsModel.getSize() > 1);
    }
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    if (processingSelection || dropList == null || nameField == null) {
      return;
    }

    try {
      processingSelection = true;
      final int selectedIndex = dropList.getSelectedIndex();
      if (selectedIndex > 0) {
        final String item = "$" + optionsModel.getElementAt(selectedIndex) + "$";
        final JTextComponent textComp = nameField;
        final int pos = textComp.getCaretPosition();
        textComp.replaceSelection(item);
        textComp.setCaretPosition(pos + item.length());
      }
    } finally {
      SwingUtilities.invokeLater(() -> {
        dropList.setSelectedIndex(0);
        processingSelection = false;
      });
    }
  }

  @Override
  public void focusGained(FocusEvent e) {
    if (dropList != null) {
      dropList.setSelectedIndex(0);
      dropList.setEnabled(true);
      if(nameField != null){
        nameField.setFocusable(textFieldFocusable);
      }
    }
  }

  @Override
  public void focusLost(FocusEvent e) {
    if (dropList != null) {
      dropList.setEnabled(false);
    }
  }

  @Override
  public void mouseClicked(MouseEvent e) {}

  @Override
  public void mousePressed(MouseEvent e) {
    if(dropList != null && dropList.getBounds().contains(e.getPoint())){
      if(nameField != null){
        nameField.setFocusable(false);
      }
    }
  }

  @Override
  public void mouseReleased(MouseEvent e) {}

  @Override
  public void mouseEntered(MouseEvent e) {}

  @Override
  public void mouseExited(MouseEvent e) {}
}