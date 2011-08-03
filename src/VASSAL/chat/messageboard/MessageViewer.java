/*
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.chat.messageboard;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
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

import VASSAL.i18n.Resources;

public class MessageViewer extends JPanel {
  private static final long serialVersionUID = 1L;

  private JTable msgTable;
  private DefaultTableModel model;
  private JTextArea msgText;
  private List<Message> msgList = new ArrayList<Message>();

  public MessageViewer() {
    initComponents();
  }

  public void setMessages(Enumeration<Message> msgEnum) {
    msgList.clear();
    msgText.setText("");  //$NON-NLS-1$
    Vector<Vector<String>> rows = new Vector<Vector<String>>();
    Vector<String> names = new Vector<String>();
    names.addElement(Resources.getString("Chat.sender"));  //$NON-NLS-1$
    names.addElement(Resources.getString("Chat.date"));  //$NON-NLS-1$
    while (msgEnum.hasMoreElements()) {
      Message msg = msgEnum.nextElement();
      msgList.add(msg);
      Vector<String> cols = new Vector<String>();
      cols.addElement(msg.getSender());
      cols.addElement(Resources.formatDate(msg.getDate()));
      rows.addElement(cols);
    }
    model = new DefaultTableModel(rows,names);
    msgTable.setModel(model);
    if (!msgList.isEmpty()) {
      msgTable.getSelectionModel().setSelectionInterval(0,0);
    }
  }

  private void initComponents() {
    JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);

    model = new DefaultTableModel(new Object[]{Resources.getString("Chat.sender"),Resources.getString("Chat.date")},0);   //$NON-NLS-1$ //$NON-NLS-2$
    msgTable = new JTable(model);
    msgTable.getSelectionModel().addListSelectionListener(new ShowMsgText());
    JScrollPane scroll = new JScrollPane(msgTable);
    split.add(scroll);

    msgText = new JTextArea(10, 25);
    msgText.setLineWrap(true);
    msgText.setWrapStyleWord(true);
    msgText.setEditable(false);
    scroll = new JScrollPane(msgText);
    scroll.setBorder(new TitledBorder(Resources.getString("Chat.message")));  //$NON-NLS-1$
    split.add(scroll);

    add(split);
  }

  private class ShowMsgText implements ListSelectionListener {
    public void valueChanged(ListSelectionEvent evt) {
      int index = msgTable.getSelectedRow();
      if (index >= 0 && index < msgList.size()) {
        msgText.setText(msgList.get(index).getText());
      }
      else {
        msgText.setText("");  //$NON-NLS-1$
      }
    }
  }
}
