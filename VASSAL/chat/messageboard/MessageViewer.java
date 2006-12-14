package VASSAL.chat.messageboard;

import java.util.Enumeration;
import java.util.Vector;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;

public class MessageViewer extends JPanel {
  private JTable msgTable;
  private DefaultTableModel model;
  private JTextArea msgText;
  private Vector msgList = new Vector();

  public MessageViewer() {
    initComponents();
  }

  public void setMessages(Enumeration msgEnum) {
    msgList.removeAllElements();
    msgText.setText("");
    Vector rows = new Vector();
    Vector names = new Vector();
    names.addElement("Sender");
    names.addElement("Date");
    while (msgEnum.hasMoreElements()) {
      Message msg = (Message) msgEnum.nextElement();
      msgList.addElement(msg);
      Vector cols = new Vector();
      cols.addElement(msg.getSender());
      cols.addElement(msg.getDate().toString());
      rows.addElement(cols);
    }
    model = new DefaultTableModel(rows,names);
    msgTable.setModel(model);
    if (msgList.size() > 0) {
      msgTable.getSelectionModel().setSelectionInterval(0,0);
    }
  }

  private void initComponents() {
    JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);

    model = new DefaultTableModel(new Object[]{"Sender","Date"},0);
    msgTable = new JTable(model);
    msgTable.getSelectionModel().addListSelectionListener(new ShowMsgText());
    JScrollPane scroll = new JScrollPane(msgTable);
    split.add(scroll);

    msgText = new JTextArea(10, 25);
    msgText.setLineWrap(true);
    msgText.setWrapStyleWord(true);
    msgText.setEditable(false);
    scroll = new JScrollPane(msgText);
    scroll.setBorder(new TitledBorder("Message"));
    split.add(scroll);

    add(split);
  }

  private class ShowMsgText implements ListSelectionListener {
    public void valueChanged(ListSelectionEvent evt) {
      int index = msgTable.getSelectedRow();
      if (index >=0 && index < msgList.size()) {
      Message msg = (Message) msgList.elementAt(index);
        msgText.setText(msg.getText());
      }
      else {
        msgText.setText("");
      }
    }
  }
}
