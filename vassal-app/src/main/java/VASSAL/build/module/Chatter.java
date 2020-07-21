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
package VASSAL.build.module;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.plaf.basic.BasicTextAreaUI;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.PlainView;
import javax.swing.text.Segment;
import javax.swing.text.TabExpander;
import javax.swing.text.Utilities;
import javax.swing.text.View;
import javax.swing.text.WrappedPlainView;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.FontConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.swing.SwingUtils;

/**
 * The chat window component.  Displays text messages and
 * accepts input.  Also acts as a {@link CommandEncoder},
 * encoding/decoding commands that display message in the text area
 */
public class Chatter extends JPanel implements CommandEncoder, Buildable {
  private static final long serialVersionUID = 1L;

  protected JTextArea conversation;
  protected JTextField input;
  protected JScrollPane scroll = new ScrollPane(
       JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
       JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
  protected static final String MY_CHAT_COLOR = "myChatColor"; //$NON-NLS-1$
  protected static final String OTHER_CHAT_COLOR = "otherChatColor"; //$NON-NLS-1$
  protected static final String GAME_MSG_COLOR = "gameMessageColor"; //$NON-NLS-1$
  protected static final String SYS_MSG_COLOR = "systemMessageColor"; //$NON-NLS-1$

  protected Color gameMsg;
  protected Color systemMsg;
  protected Color myChat;
  protected Color otherChat;

  public static String getAnonymousUserName() {
    return Resources.getString("Chat.anonymous"); //$NON-NLS-1$
  }

  public Chatter() {
    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    conversation = new JTextArea(10, 80);
    for (int i = 0; i < 10; ++i) {
      conversation.append("\n"); //$NON-NLS-1$
    }
    conversation.setEditable(false);
    conversation.setLineWrap(true);
    conversation.setWrapStyleWord(true);
    conversation.setUI(new UI());
    conversation.addComponentListener(new ComponentAdapter() {
      @Override
      public void componentResized(ComponentEvent e) {
        scroll.getVerticalScrollBar().setValue(scroll.getVerticalScrollBar().getMaximum());
      }
    });
    input = new JTextField(80);
    input.setFocusTraversalKeysEnabled(false);
    input.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        send(formatChat(e.getActionCommand()));
        input.setText(""); //$NON-NLS-1$
      }
    });
    input.setMaximumSize(new Dimension(input.getMaximumSize().width,
                                       input.getPreferredSize().height));
    scroll.setViewportView(conversation);
    add(scroll);
    add(input);
  }

  private String formatChat(String text) {
    final String id = GlobalOptions.getInstance().getPlayerId();
    return "<" + (id.length() == 0 ? "("+getAnonymousUserName()+")" : id) + "> - " + text; //$NON-NLS-1$ //$NON-NLS-2$
  }

  public JTextField getInputField() {
    return input;
  }

  /**
   * Display a message in the text area
   */
  public void show(String s) {
    conversation.append("\n" + s); //$NON-NLS-1$
  }

  /** @deprecated use GlobalOptions.getPlayerId() */
  @Deprecated public void setHandle(String s) {
  }

  /** @deprecated use GlobalOptions.getPlayerId() */
  @Deprecated public String getHandle() {
    return GlobalOptions.getInstance().getPlayerId();
  }

  /**
   * Set the Font used by the text area
   */
  @Override
  public void setFont(Font f) {
    if (input != null) {
      if (input.getText().length() == 0) {
        input.setText("XXX"); //$NON-NLS-1$
        input.setFont(f);
        input.setText(""); //$NON-NLS-1$
      }
      else
        input.setFont(f);
    }
    if (conversation != null) {
      conversation.setFont(f);
    }
  }

  @Override
  public void build(org.w3c.dom.Element e) {
  }

  @Override
  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  /**
   * Expects to be added to a GameModule.  Adds itself to the
   * controls window and registers itself as a
   * {@link CommandEncoder} */
  @Override
  public void addTo(Buildable b) {
    GameModule mod = (GameModule) b;
    mod.setChatter(this);
    mod.addCommandEncoder(this);
    mod.addKeyStrokeSource(new KeyStrokeSource(this, WHEN_ANCESTOR_OF_FOCUSED_COMPONENT));

    final FontConfigurer chatFont = new FontConfigurer(
      "ChatFont", Resources.getString("Chatter.chat_font_preference")
    );
    chatFont.addPropertyChangeListener(new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent evt) {
        setFont((Font) evt.getNewValue());
      }
    });

    mod.getControlPanel().add(this, BorderLayout.CENTER);

    chatFont.fireUpdate();
    mod.getPrefs().addOption(Resources.getString("Chatter.chat_window"), chatFont); //$NON-NLS-1$

    //Bug 10179 - Do not re-read Chat colors each time the Chat Window is repainted.
    final Prefs globalPrefs = Prefs.getGlobalPrefs();

    //
    // game message color
    //
    final ColorConfigurer gameMsgColor = new ColorConfigurer(
      GAME_MSG_COLOR,
      Resources.getString("Chatter.game_messages_preference"),
      Color.magenta
    );

    gameMsgColor.addPropertyChangeListener(new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent e) {
        gameMsg = (Color) e.getNewValue();
      }
    });

    globalPrefs.addOption(
      Resources.getString("Chatter.chat_window"), gameMsgColor
    );

    gameMsg = (Color) globalPrefs.getValue(GAME_MSG_COLOR);

    //
    // system message color
    //
    final ColorConfigurer systemMsgColor = new ColorConfigurer(
      SYS_MSG_COLOR,
      Resources.getString("Chatter.system_message_preference"),
      new Color(160, 160, 160)
    );

    systemMsgColor.addPropertyChangeListener(new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent e) {
        systemMsg = (Color) e.getNewValue();
      }
    });

    globalPrefs.addOption(
      Resources.getString("Chatter.chat_window"), systemMsgColor
    );

    systemMsg = (Color) globalPrefs.getValue(SYS_MSG_COLOR);

    //
    // my message color
    //
    final ColorConfigurer myChatColor = new ColorConfigurer(
      MY_CHAT_COLOR,
      Resources.getString("Chatter.my_text_preference"),
      Color.gray
    );

    myChatColor.addPropertyChangeListener(new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent e) {
        myChat = (Color) e.getNewValue();
      }
    });

    globalPrefs.addOption(
      Resources.getString("Chatter.chat_window"), myChatColor
    );

    myChat = (Color) globalPrefs.getValue(MY_CHAT_COLOR);

    //
    // other message color
    //
    final ColorConfigurer otherChatColor = new ColorConfigurer(
      OTHER_CHAT_COLOR,
      Resources.getString("Chatter.other_text_preference"),
      Color.black
    );

    otherChatColor.addPropertyChangeListener(new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent e) {
        otherChat = (Color) e.getNewValue();
      }
    });

    globalPrefs.addOption(
      Resources.getString("Chatter.chat_window"), otherChatColor
    );

    otherChat = (Color) globalPrefs.getValue(OTHER_CHAT_COLOR);
  }

  @Override
  public void add(Buildable b) {
  }

  @Override
  public Command decode(String s) {
    if (s.startsWith("CHAT")) { //$NON-NLS-1$
      return new DisplayText(this, s.substring(4));
    }
    else {
      return null;
    }
  }

  @Override
  public String encode(Command c) {
    if (c instanceof DisplayText) {
      return "CHAT" + ((DisplayText) c).msg; //$NON-NLS-1$
    }
    else {
      return null;
    }
  }

  /**
   * Displays the message, Also logs and sends to the server
   * a {@link Command} that displays this message
   */
  public void send(String msg) {
    if (msg != null
        && msg.length() > 0) {
      show(msg);
      GameModule.getGameModule().sendAndLog(new DisplayText(this, msg));
    }
  }


  /**
   * Classes other than the Chatter itself may forward KeyEvents
   * to the Chatter by using this method
   */
  public void keyCommand(KeyStroke e) {
    if ((e.getKeyCode() == 0 || e.getKeyCode() == KeyEvent.CHAR_UNDEFINED)
        && !Character.isISOControl(e.getKeyChar())) {
      input.setText(input.getText() + e.getKeyChar());
    }
    else if (e.isOnKeyRelease()) {
      switch (e.getKeyCode()) {
      case KeyEvent.VK_ENTER:
        if (input.getText().length() > 0)
          send(formatChat(input.getText()));
        input.setText(""); //$NON-NLS-1$
        break;
      case KeyEvent.VK_BACK_SPACE:
      case KeyEvent.VK_DELETE:
        String s = input.getText();
        if (s.length() > 0)
          input.setText(s.substring(0, s.length() - 1));
        break;
      }
    }
  }

  private class UI extends BasicTextAreaUI {
    @Override
    public View create(javax.swing.text.Element elem) {
      JTextComponent c = getComponent();
      if (c instanceof JTextArea) {
        JTextArea area = (JTextArea) c;
        View v;
        if (area.getLineWrap()) {
          v = new WrappedView(elem, area.getWrapStyleWord());
        }
        else {
          v = new PView(elem);
        }
        return v;
      }
      return null;
    }
  }

  private float drawColoredText(Graphics g, float x, float y, TabExpander ex, Document doc,
                              int p0, int p1, Element elem) throws BadLocationException {
    final Segment s = new Segment();
    doc.getText(p0, p1 - p0, s);
    g.setColor(getColor(elem));

    final Graphics2D g2d = (Graphics2D) g;
    g2d.addRenderingHints(SwingUtils.FONT_HINTS);
    return Utilities.drawTabbedText(s, x, y, g2d, ex, p0);
  }

  private class WrappedView extends WrappedPlainView {
    private WrappedView(Element el, boolean wrap) {
      super(el, wrap);
    }

    @Override
    protected float drawUnselectedText(Graphics2D g, float x, float y,
                                     int p0, int p1) throws BadLocationException {
      final Element root = getElement();
      return drawColoredText(g, x, y, this, getDocument(), p0, p1, root.getElement(root.getElementIndex(p0)));
    }
  }

  private class PView extends PlainView {
    private PView(Element el) {
      super(el);
    }

    @Override
    protected float drawUnselectedText(Graphics2D g, float x, float y,
                                     int p0, int p1) throws BadLocationException {
      Element root = getElement();
      return drawColoredText(g, x, y, this, getDocument(), p0, p1, root.getElement(root.getElementIndex(p0)));
    }
  }

  /**
   * Determines the color with which to draw a given line of text
   * @return the Color to draw
   */
  protected Color getColor(Element elem) {
    Color col = null;
    try {
      final String s = elem.getDocument().getText(
        elem.getStartOffset(), elem.getEndOffset() - elem.getStartOffset()
      ).trim();

      if (s.length() > 0) {
        switch (s.charAt(0)) {
        case '*':
          col = gameMsg;
          break;
        case '-':
          col = systemMsg;
          break;
        default:
          if (s.startsWith(formatChat(""))) { //$NON-NLS-1$
            col = myChat;
          }
          else {
            col = otherChat;
          }
          break;
        }
      }
    }
    catch (BadLocationException e) {
      ErrorDialog.bug(e);
    }

    return col == null ? Color.black : col;
  }

  /**
   * This is a {@link Command} object that, when executed, displays
   * a text message in the Chatter's text area     */
  public static class DisplayText extends Command {
    private String msg;
    private Chatter c;

    public DisplayText(Chatter c, String s) {
      this.c = c;
      msg = s;
      if (msg.startsWith("<>")) {
        msg = "<(" + Chatter.getAnonymousUserName() + ")>" + s.substring(2);
      }
      else {
        msg = s;
      }
    }

    @Override
    public void executeCommand() {
      c.show(msg);
    }

    @Override
    public Command myUndoCommand() {
      return new DisplayText(c, Resources.getString("Chatter.undo_message", msg)); //$NON-NLS-1$
    }

    public String getMessage() {
      return msg;
    }

    @Override
    public String getDetails() {
      return msg;
    }
  }

  public static void main(String[] args) {
    Chatter chat = new Chatter();
    JFrame f = new JFrame();
    f.add(chat);
    f.pack();
    f.setVisible(true);
  }
}
