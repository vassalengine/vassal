/*
 *
 * Copyright (c) 2000-2020 by Rodney Kinney, Brian Reynolds, VASSAL
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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.LinkedHashSet;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.TransferHandler;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyledDocument;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;
import javax.swing.text.html.parser.ParserDelegator;

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
import VASSAL.tools.QuickColors;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.swing.DataArchiveHTMLEditorKit;

/**
 * The chat window component. Displays text messages and accepts input. Also
 * acts as a {@link CommandEncoder}, encoding/decoding commands that display
 * message in the text area
 */
public class Chatter extends JPanel implements CommandEncoder, Buildable {
  private static final long serialVersionUID = 1L;

  protected JTextPane conversationPane;
  protected HTMLDocument doc;
  protected HTMLEditorKit kit;
  protected StyleSheet style;

  protected JTextField input;
  protected JScrollPane scroll = new ScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
  protected JScrollPane scroll2 = new ScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
  protected static final String MY_CHAT_COLOR = "HTMLChatColor";          //$NON-NLS-1$ // Different tags to "restart" w/ new default scheme
  protected static final String OTHER_CHAT_COLOR = "HTMLotherChatColor";     //$NON-NLS-1$
  protected static final String GAME_MSG1_COLOR = "HTMLgameMessage1Color";  //$NON-NLS-1$
  protected static final String GAME_MSG2_COLOR = "HTMLgameMessage2Color";  //$NON-NLS-1$
  protected static final String GAME_MSG3_COLOR = "HTMLgameMessage3Color";  //$NON-NLS-1$
  protected static final String GAME_MSG4_COLOR = "HTMLgameMessage4Color";  //$NON-NLS-1$
  protected static final String GAME_MSG5_COLOR = "HTMLgameMessage5Color";  //$NON-NLS-1$
  protected static final String SYS_MSG_COLOR = "HTMLsystemMessageColor"; //$NON-NLS-1$

  protected Font myFont;

  protected Color gameMsg, gameMsg2, gameMsg3, gameMsg4, gameMsg5;
  protected Color systemMsg, myChat, otherChat;
  protected boolean needUpdate;

  protected JTextArea conversation;    // Backward compatibility for overridden classes. Needs something to suppress.

  public static String getAnonymousUserName() {
    return Resources.getString("Chat.anonymous"); //$NON-NLS-1$
  }

  public Chatter() {
    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

    conversation = new JTextArea(); // For backward override compatibility only.

    //BR// Conversation is now a JTextPane w/ HTMLEditorKit to process HTML, which gives us HTML support "for free".
    conversationPane = new JTextPane();
    conversationPane.setContentType("text/html"); //NON-NLS
    kit = new DataArchiveHTMLEditorKit(GameModule.getGameModule().getDataArchive());
    conversationPane.setEditorKit(kit);

    doc = (HTMLDocument) conversationPane.getDocument();

    style = kit.getStyleSheet();
    myFont = new Font("SansSerif", Font.PLAIN, 12); //NON-NLS // Will be overridden by the font from Chat preferences

    try {
      for (int i = 0; i < 30; ++i) { // Scroll past some arbitrary number of empty lines so that first text will start appearing on the bottom line
        kit.insertHTML(doc, doc.getLength(), "<br>", 0, 0, null); //NON-NLS
      }
    }
    catch (BadLocationException | IOException ble) {
      ErrorDialog.bug(ble);
    }

    conversationPane.setEditable(false);
    conversationPane.addComponentListener(new ComponentAdapter() {
      @Override
      public void componentResized(ComponentEvent e) {
        scroll.getVerticalScrollBar().setValue(scroll.getVerticalScrollBar().getMaximum());
      }
    });

    input = new JTextField(60);
    input.setFocusTraversalKeysEnabled(false);
    input.addActionListener(e -> {
      send(formatChat(e.getActionCommand()), e.getActionCommand());
      input.setText(""); //$NON-NLS-1$
    });
    input.setMaximumSize(new Dimension(input.getMaximumSize().width, input.getPreferredSize().height));

    final FontMetrics fm = getFontMetrics(myFont);
    final int fontHeight = fm.getHeight();

    conversationPane.setPreferredSize(new Dimension(input.getMaximumSize().width, fontHeight * 10));

    scroll.setViewportView(conversationPane);
    scroll.getVerticalScrollBar().setUnitIncrement(input.getPreferredSize().height); //Scroll this faster
    add(scroll);
    add(input);

    final HTMLCopier transfer = new HTMLCopier();
    //final StyleTransferHandler transfer = new StyleTransferHandler();
    conversationPane.setTransferHandler(transfer);

    final Action copyAction = new DefaultEditorKit.CopyAction();
    copyAction.putValue(Action.NAME, "Copy");

    //final Action copyAllAction = new CopyAction(conversationPane);

    final JPopupMenu pm = new JPopupMenu();
    pm.add(copyAction);
    //pm.add(copyAllAction);

    conversationPane.addMouseListener(new MouseAdapter() {

      @Override
      public void mouseClicked(MouseEvent e) {
        if (e.isPopupTrigger()) {
          doPopup(e);
        }
      }

      @Override
      public void mouseReleased(MouseEvent e) {
        if (e.isPopupTrigger()) {
          doPopup(e);
        }
      }

      protected void doPopup(MouseEvent e) {
        pm.show(e.getComponent(), e.getX(), e.getY());
      }

    });


    setPreferredSize(new Dimension(input.getMaximumSize().width, input.getPreferredSize().height + conversationPane.getPreferredSize().height));
  }


  public class CopyAction extends AbstractAction {
    private JTextPane pane;

    public CopyAction(JTextPane pane) {
      this.pane = pane;
      putValue(NAME, "Copy");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      //JComponent target = getTextComponent
      //JTextComponent target = getTextComponent(e);
      //if (target != null) {
      //  target.copy();
      //}

      //ActionEvent.

      //pane.getActionMap().get("copy").actionPerformed(new ActionEvent(pane, e.getID(), ""));
    }
  }

  /**
   * Because our Chatters make themselves visible in their constructor, providing a way for an overriding class to
   * "turn this chatter off" is friendlier than What Went Before.
   *
   * @param vis - whether this chatter should be visible
   */
  protected void setChatterVisible(boolean vis) {
    conversationPane.setVisible(vis);
    input.setVisible(vis);
    scroll.setVisible(vis);
  }


  protected String formatChat(String text) {
    final String id = GlobalOptions.getInstance().getPlayerId();
    return String.format("&lt;%s&gt; - %s", id.isEmpty() ? "(" + getAnonymousUserName() + ")" : id, text); //HTML-friendly angle brackets //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }

  public JTextField getInputField() {
    return input;
  }

  /**
   * Styles a chat message based on the player who sent it. Presently just distinguishes a chat message "from me" from a chat message "from anyone else".
   * <p>
   * To make the player colors easy to override in a custom class
   * (my modules have logic to assign individual player colors -- beyond the scope of the present effort but a perhaps a fun future addition)
   *
   * @param s - chat message from a player
   * @return - an entry in our CSS style sheet to use for this chat message
   */
  protected String getChatStyle(String s) {
    return s.startsWith(formatChat("").trim()) ? "mychat" : "other"; //NON-NLS
  }

  /**
   * A hook for inserting a console class that accepts commands
   *
   * @param s            - chat message
   * @param style        - current style name (contains information that might be useful)
   * @param html_allowed - flag if html_processing is enabled for this message (allows console to apply security considerations)
   * @return true        - if was accepted as a console command
   */
  @SuppressWarnings("unused")
  public boolean consoleHook(String s, String style, boolean html_allowed) {
    return GameModule.getGameModule().getConsole().exec(s, style, html_allowed);
  }


  /**
   * Display a message in the text area. Ensures we execute in the EDT
   */
  public void show(String s) {
    if (SwingUtilities.isEventDispatchThread()) {
      doShow(s);
    }
    else {
      SwingUtilities.invokeLater(() -> doShow(s));
    }
  }


  /**
   * Display a message in the text area - use show() from outside the class - MUST run on EventDispatchThread
   */
  private void doShow(String s) {
    final String style;
    final boolean html_allowed;

    // Choose an appropriate style to display this message in
    s = s.trim();
    if (!s.isEmpty()) {
      if (s.startsWith("*")) {
        html_allowed = (QuickColors.getQuickColor(s, "*") >= 0) || GlobalOptions.getInstance().chatterHTMLSupport();
        style = QuickColors.getQuickColorHTMLStyle(s, "*");
        s = QuickColors.stripQuickColorTag(s, "*");
      }
      else if (s.startsWith("-")) {
        html_allowed = true;
        style = (QuickColors.getQuickColor(s, "-") >= 0) ? QuickColors.getQuickColorHTMLStyle(s, "-") : "sys"; //NON-NLS
        s = QuickColors.stripQuickColorTag(s, "-");
      }
      else {
        style = getChatStyle(s);
        html_allowed = false;
      }
    }
    else {
      style = "msg";  //NON-NLS
      html_allowed = false;
    }

    // Disable unwanted HTML tags in contexts where it shouldn't be allowed:
    // (1) Anything received from chat channel, for security reasons
    // (2) Legacy module "report" text when not explicitly opted in w/ first character or preference setting
    if (!html_allowed) {
      s = s.replaceAll("<", "&lt;")  //NON-NLS // This prevents any unwanted tag from functioning
        .replaceAll(">", "&gt;"); //NON-NLS // This makes sure > doesn't break any of our legit <div> tags
    }

    // Now we have to fix up any legacy angle brackets around the word <observer>
    final String keystring = Resources.getString("PlayerRoster.observer");
    final String replace = keystring.replace("<", "&lt;").replace(">", "&gt;"); //NON-NLS
    if (!replace.equals(keystring)) {
      s = s.replace(keystring, replace);
    }

    // Insert a div of the correct style for our line of text. Module designer
    // still free to insert <span> tags and <img> tags and the like in Report
    // messages.
    try {
      kit.insertHTML(doc, doc.getLength(), "\n<div class=" + style + ">" + s + "</div>", 0, 0, null); //NON-NLS
    }
    catch (BadLocationException | IOException ble) {
      ErrorDialog.bug(ble);
    }

    conversationPane.repaint();

    //consoleHook(s, style, html_allowed);
  }

  /**
   * Adds or updates a CSS stylesheet entry. Styles in the color, font type, and font size.
   *
   * @param s           Style name
   * @param f           Font to use
   * @param c           Color for text
   * @param font_weight Bold? Italic?
   * @param size        Font size
   */
  protected void addStyle(String s, Font f, Color c, String font_weight, int size) {
    if ((style == null) || (c == null)) return;
    style.addRule(s +
      " {color:" +                                                               //NON-NLS
      String.format("#%02x%02x%02x", c.getRed(), c.getGreen(), c.getBlue()) +    //NON-NLS
      "; font-family:" +                                                         //NON-NLS
      f.getFamily() +
      "; font-size:" +                                                           //NON-NLS
      (size > 0 ? size : f.getSize()) +
      "; " +                                                                     //NON-NLS
      ((!font_weight.isBlank()) ? "font-weight:" + font_weight + "; " : "") +    //NON-NLS
      "}");                                                                      //NON-NLS

    style.addRule(s + "color {color:" + String.format("#%02x%02x%02x", c.getRed(), c.getGreen(), c.getBlue()) + "; }"); //NON-NLS
  }

  /**
   * Build ourselves a CSS stylesheet from our preference font/color settings.
   *
   * @param f - Font to use for this stylesheet
   */
  protected void makeStyleSheet(Font f) {
    if (style == null) {
      return;
    }

    if (f == null) {
      if (myFont == null) {
        f = new Font("SansSerif", Font.PLAIN, 12); //NON-NLS
        myFont = f;
      }
      else {
        f = myFont;
      }
    }
    addStyle(".msg", f, gameMsg, "", 0); //NON-NLS
    addStyle(".msg2", f, gameMsg2, "", 0); //NON-NLS
    addStyle(".msg3", f, gameMsg3, "", 0); //NON-NLS
    addStyle(".msg4", f, gameMsg4, "", 0); //NON-NLS
    addStyle(".msg5", f, gameMsg5, "", 0); //NON-NLS
    addStyle(".mychat", f, myChat, "bold", 0); //NON-NLS
    addStyle(".other ", f, otherChat, "bold", 0); //NON-NLS
    addStyle(".sys", f, systemMsg, "", 0); //NON-NLS

    // A fun extension would be letting the module designer provide extra class styles.
  }

  /**
   * Set the Font used by the text area
   */
  @Override
  public void setFont(Font f) {
    myFont = f;
    if (input != null) {
      if (input.getText().isEmpty()) {
        input.setText("XXX"); //$NON-NLS-1$
        input.setFont(f);
        input.setText(""); //$NON-NLS-1$
      }
      else {
        input.setFont(f);
      }
    }
    if (conversationPane != null) {
      conversationPane.setFont(f);
    }
    makeStyleSheet(f); // When font changes, rebuild our stylesheet
  }

  @Override
  public void build(org.w3c.dom.Element e) {
  }

  @Override
  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  /**
   * Expects to be added to a GameModule. Adds itself to the controls window and
   * registers itself as a {@link CommandEncoder}
   */
  @Override
  public void addTo(Buildable b) {
    final GameModule mod = (GameModule) b;
    mod.setChatter(this);
    mod.addCommandEncoder(this);
    mod.addKeyStrokeSource(new KeyStrokeSource(this, WHEN_ANCESTOR_OF_FOCUSED_COMPONENT));

    final FontConfigurer chatFont = new FontConfigurer("ChatFont", //NON-NLS
      Resources.getString("Chatter.chat_font_preference"));

    chatFont.addPropertyChangeListener(evt -> setFont((Font) evt.getNewValue()));

    mod.getPlayerWindow().addChatter(this);

    chatFont.fireUpdate();
    mod.getPrefs().addOption(Resources.getString("Chatter.chat_window"), chatFont); //$NON-NLS-1$

    // Bug 10179 - Do not re-read Chat colors each time the Chat Window is
    // repainted.
    final Prefs globalPrefs = Prefs.getGlobalPrefs();

    //
    // game message color
    //
    final ColorConfigurer gameMsgColor = new ColorConfigurer(GAME_MSG1_COLOR,
      Resources.getString("Chatter.game_messages_preference"), Color.black);

    gameMsgColor.addPropertyChangeListener(e -> {
      gameMsg = (Color) e.getNewValue();
      makeStyleSheet(null);
    });

    globalPrefs.addOption(Resources.getString("Chatter.chat_window"), gameMsgColor);

    gameMsg = (Color) globalPrefs.getValue(GAME_MSG1_COLOR);

    // game message color #2 (messages starting with "!")
    final ColorConfigurer gameMsg2Color = new ColorConfigurer(GAME_MSG2_COLOR,
      Resources.getString("Chatter.game_messages_preference_2"), new Color(0, 153, 51));

    gameMsg2Color.addPropertyChangeListener(e -> {
      gameMsg2 = (Color) e.getNewValue();
      makeStyleSheet(null);
    });

    globalPrefs.addOption(Resources.getString("Chatter.chat_window"), gameMsg2Color);

    gameMsg2 = (Color) globalPrefs.getValue(GAME_MSG2_COLOR);

    // game message color #3 (messages starting with "?")
    final ColorConfigurer gameMsg3Color = new ColorConfigurer(GAME_MSG3_COLOR,
      Resources.getString("Chatter.game_messages_preference_3"), new Color(255, 102, 102));

    gameMsg3Color.addPropertyChangeListener(e -> {
      gameMsg3 = (Color) e.getNewValue();
      makeStyleSheet(null);
    });

    globalPrefs.addOption(Resources.getString("Chatter.chat_window"), gameMsg3Color);

    gameMsg3 = (Color) globalPrefs.getValue(GAME_MSG3_COLOR);


    // game message color #4 (messages starting with "~")
    final ColorConfigurer gameMsg4Color = new ColorConfigurer(GAME_MSG4_COLOR,
      Resources.getString("Chatter.game_messages_preference_4"), new Color(255, 0, 0));

    gameMsg4Color.addPropertyChangeListener(e -> {
      gameMsg4 = (Color) e.getNewValue();
      makeStyleSheet(null);
    });

    globalPrefs.addOption(Resources.getString("Chatter.chat_window"), gameMsg4Color);

    gameMsg4 = (Color) globalPrefs.getValue(GAME_MSG4_COLOR);


    // game message color #5 (messages starting with "`")
    final ColorConfigurer gameMsg5Color = new ColorConfigurer(GAME_MSG5_COLOR,
      Resources.getString("Chatter.game_messages_preference_5"), new Color(153, 0, 153));

    gameMsg5Color.addPropertyChangeListener(e -> {
      gameMsg5 = (Color) e.getNewValue();
      makeStyleSheet(null);
    });

    globalPrefs.addOption(Resources.getString("Chatter.chat_window"), gameMsg5Color);

    gameMsg5 = (Color) globalPrefs.getValue(GAME_MSG5_COLOR);


    final ColorConfigurer systemMsgColor = new ColorConfigurer(SYS_MSG_COLOR,
      Resources.getString("Chatter.system_message_preference"), new Color(160, 160, 160));

    systemMsgColor.addPropertyChangeListener(e -> {
      systemMsg = (Color) e.getNewValue();
      makeStyleSheet(null);
    });

    globalPrefs.addOption(Resources.getString("Chatter.chat_window"), systemMsgColor);

    systemMsg = (Color) globalPrefs.getValue(SYS_MSG_COLOR);


    final ColorConfigurer myChatColor = new ColorConfigurer(
      MY_CHAT_COLOR,
      Resources.getString("Chatter.my_text_preference"),
      new Color(9, 32, 229));

    myChatColor.addPropertyChangeListener(e -> {
      myChat = (Color) e.getNewValue();
      makeStyleSheet(null);
    });

    globalPrefs.addOption(Resources.getString("Chatter.chat_window"), myChatColor);

    myChat = (Color) globalPrefs.getValue(MY_CHAT_COLOR);

    final ColorConfigurer otherChatColor = new ColorConfigurer(
      OTHER_CHAT_COLOR,
      Resources.getString("Chatter.other_text_preference"),
      new Color(0, 153, 255)
    );

    otherChatColor.addPropertyChangeListener(e -> {
      otherChat = (Color) e.getNewValue();
      makeStyleSheet(null);
    });

    globalPrefs.addOption(Resources.getString("Chatter.chat_window"), otherChatColor);
    otherChat = (Color) globalPrefs.getValue(OTHER_CHAT_COLOR);

    makeStyleSheet(myFont);
  }

  @Override
  public void add(Buildable b) {
  }

  @Override
  public Command decode(String s) {
    if (!s.startsWith(DisplayText.PREFIX)) {
      return null;
    }
    return new DisplayText(this, s.substring(DisplayText.PREFIX.length()));
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof DisplayText)) {
      return null;
    }
    return DisplayText.PREFIX + ((DisplayText) c).getMessage();
  }


  /**
   * Displays the message, Also logs and sends to the server a {@link Command}
   * that displays this message.
   */
  public void send(String msg) {
    if (msg != null && !msg.isEmpty()) {
      show(msg);
      GameModule.getGameModule().sendAndLog(new DisplayText(this, msg));
    }
  }

  /**
   * Checks first for an intercepted console command; otherwise displays the message
   *
   * @param msg     message to display if not a console command
   * @param console potential console command (without any chat livery added to it)
   */
  public void send(String msg, String console) {
    if (!consoleHook(console, "", false)) {
      send(msg);
    }
  }

  /**
   * Warning message method -- same as send, but accepts messages from static methods. For reporting soft-fail problems in modules.
   */
  public static void warning(String msg) {
    final Chatter chatter = GameModule.getGameModule().getChatter();
    chatter.send(msg);
  }


  /**
   * Classes other than the Chatter itself may forward KeyEvents to the Chatter by
   * using this method
   */
  public void keyCommand(KeyStroke e) {

    if ((e.getKeyCode() == 0 || e.getKeyCode() == KeyEvent.CHAR_UNDEFINED)
      && !Character.isISOControl(e.getKeyChar())) {
      if ((e.getModifiers() & (KeyEvent.ALT_DOWN_MASK | KeyEvent.CTRL_DOWN_MASK | KeyEvent.ALT_GRAPH_DOWN_MASK)) != 0) {
        return;  // Do not report keystrokes with Ctrl/Alt on. These get through to here on Macs
      }
      input.setText(input.getText() + e.getKeyChar());
    }
    else if (e.isOnKeyRelease()) {
      switch (e.getKeyCode()) {
      case KeyEvent.VK_ENTER:
        if (!input.getText().isEmpty())
          send(formatChat(input.getText()), input.getText());
        input.setText(""); //$NON-NLS-1$
        break;
      case KeyEvent.VK_BACK_SPACE:
      case KeyEvent.VK_DELETE:
        final String s = input.getText();
        if (!s.isEmpty())
          input.setText(s.substring(0, s.length() - 1));
        break;
      }
    }
  }


  /**
   * This is a {@link Command} object that, when executed, displays
   * a text message in the Chatter's text area
   */
  public static class DisplayText extends Command {

    public static final String PREFIX = "CHAT";  //$NON-NLS-1$

    private String msg;
    private final Chatter c;

    public DisplayText(Chatter c, String s) {
      this.c = c;
      msg = s;
      if (msg.startsWith("<>")) { //NON-NLS
        msg = "&lt;(" + Chatter.getAnonymousUserName() + ")&gt;" + s.substring(2); // HTML-friendly //NON-NLS
        // angle brackets
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
    final Chatter chat = new Chatter();
    final JFrame f = new JFrame();
    f.add(chat);
    f.pack();
    f.setVisible(true);
  }

  /**
   * @deprecated use {@link GlobalOptions#getPlayerId()}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public String getHandle() {
    return GlobalOptions.getInstance().getPlayerId();
  }


  static class ChatTransferHandler extends TransferHandler {

    protected Transferable createTransferable(JComponent c) {
      final JEditorPane pane = (JEditorPane) c;
      final String htmlText = pane.getText();
      final String plainText = extractText(new StringReader(htmlText));
      return new ChatTransferable(plainText, htmlText);
    }

    public String extractText(Reader reader) {
      final ArrayList<String> list = new ArrayList<String>();

      final HTMLEditorKit.ParserCallback parserCallback = new HTMLEditorKit.ParserCallback() {
        public void handleText(final char[] data, final int pos) {
          list.add(new String(data));
        }

        public void handleStartTag(HTML.Tag tag, MutableAttributeSet attribute, int pos) {
        }

        public void handleEndTag(HTML.Tag t, final int pos) {
        }

        public void handleSimpleTag(HTML.Tag t, MutableAttributeSet a, final int pos) {
          if (t.equals(HTML.Tag.BR)) {
            list.add("\n");
          }
        }

        public void handleComment(final char[] data, final int pos) {
        }

        public void handleError(final String errMsg, final int pos) {
        }
      };
      try {
        new ParserDelegator().parse(reader, parserCallback, true);
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      String result = "";
      for (final String s : list) {
        result += s;
      }
      return result;
    }

    @Override
    public void exportToClipboard(JComponent comp, Clipboard clip, int action) throws IllegalStateException {
      if (action == COPY) {
        clip.setContents(this.createTransferable(comp), null);
      }
    }

    @Override
    public int getSourceActions(JComponent c) {
      return COPY;
    }
  }

  static class ChatTransferable implements Transferable {
    private final DataFlavor[] supportedFlavors;

    private final String plainData;
    private final String htmlData;

    public ChatTransferable(String plainData, String htmlData) {
      this.plainData = plainData;
      this.htmlData = htmlData;

      try {
        supportedFlavors = new DataFlavor[]{
          new DataFlavor("text/html;class=java.lang.String"),
          new DataFlavor("text/plain;class=java.lang.String")
        };
      }
      catch (ClassNotFoundException e) {
        throw new ExceptionInInitializerError(e);
      }
    }

    public DataFlavor[] getTransferDataFlavors() {
      return supportedFlavors;
    }

    public boolean isDataFlavorSupported(DataFlavor flavor) {
      for (final DataFlavor supportedFlavor : supportedFlavors) {
        if (supportedFlavor == flavor) {
          return true;
        }
      }
      return false;
    }

    public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
      if (flavor.equals(supportedFlavors[0])) {
        return htmlData;
      }
      if (flavor.equals(supportedFlavors[1])) {
        return plainData;
      }
      throw new UnsupportedFlavorException(flavor);
    }
  }

  public class HTMLCopier
    extends TransferHandler {
    private static final long serialVersionUID = 1;

    private final Collection<DataFlavor> flavors;

    HTMLCopier() {
      Collection<DataFlavor> flavorList = new LinkedHashSet<>();
      Collections.addAll(flavorList,
        new DataFlavor(String.class, null),
        DataFlavor.stringFlavor);

      String[] mimeTypes = {
        "text/html", "text/plain"
      };
      Class<?>[] textClasses = {
        Reader.class, String.class, CharBuffer.class, char[].class
      };
      Class<?>[] byteClasses = {
        InputStream.class, ByteBuffer.class, byte[].class
      };
      String[] charsets = {
        Charset.defaultCharset().name(),
        StandardCharsets.UTF_8.name(),
        StandardCharsets.UTF_16.name(),
        StandardCharsets.UTF_16BE.name(),
        StandardCharsets.UTF_16LE.name(),
        StandardCharsets.ISO_8859_1.name(),
        "windows-1252",
        StandardCharsets.US_ASCII.name(),
      };

      try {
        flavorList.add(new DataFlavor(
          DataFlavor.javaJVMLocalObjectMimeType +
            "; class=" + String.class.getName()));

        for (String mimeType : mimeTypes) {
          for (Class<?> textClass : textClasses) {
            flavorList.add(new DataFlavor(String.format(
              "%s; class=\"%s\"",
              mimeType, textClass.getName())));
          }
          for (String charset : charsets) {
            for (Class<?> byteClass : byteClasses) {
              flavorList.add(new DataFlavor(String.format(
                "%s; charset=%s; class=\"%s\"",
                mimeType, charset, byteClass.getName())));
            }
          }
        }

        for (String mimeType : mimeTypes) {
          flavorList.add(new DataFlavor(String.format(
            "%s; charset=unicode; class=\"%s\"",
            mimeType, InputStream.class.getName())));
        }
      }
      catch (ClassNotFoundException e) {
        throw new RuntimeException(e);
      }

      this.flavors = Collections.unmodifiableCollection(flavorList);
    }

    @Override
    public int getSourceActions(JComponent component) {
      return COPY_OR_MOVE;
    }

    @Override
    public void exportToClipboard(JComponent component,
                                  Clipboard clipboard,
                                  int action) {
      JTextPane pane = (JTextPane) component;
      Document doc = pane.getDocument();

      int start = pane.getSelectionStart();
      int end = pane.getSelectionEnd();

      final String html;
      final String plainText;
      try {
        StringWriter writer = new StringWriter(end - start);
        pane.getEditorKit().write(writer, doc, start, end - start);
        html = writer.toString();

        StringBuilder plainTextBuilder = new StringBuilder();
        appendTextContent(doc.getDefaultRootElement(), start, end,
          plainTextBuilder);
        plainText = plainTextBuilder.toString();
      }
      catch (BadLocationException | IOException e) {
        throw new RuntimeException(e);
      }

      Transferable contents = new Transferable() {
        @Override
        public boolean isDataFlavorSupported(DataFlavor flavor) {
          return flavors.contains(flavor);
        }

        @Override
        public DataFlavor[] getTransferDataFlavors() {
          return flavors.toArray(new DataFlavor[0]);
        }

        @Override
        public Object getTransferData(DataFlavor flavor)
          throws UnsupportedFlavorException,
          IOException {

          String data;
          if (flavor.isMimeTypeEqual("text/html")) {
            data = html;
          }
          else {
            data = plainText;
          }

          Class<?> dataClass = flavor.getRepresentationClass();
          if (dataClass.equals(char[].class)) {
            return data.toCharArray();
          }
          if (flavor.isRepresentationClassReader()) {
            return new StringReader(data);
          }
          if (flavor.isRepresentationClassCharBuffer()) {
            return CharBuffer.wrap(data);
          }
          if (flavor.isRepresentationClassByteBuffer()) {
            String charset = flavor.getParameter("charset");
            return Charset.forName(charset).encode(data);
          }
          if (flavor.isRepresentationClassInputStream()) {
            String charset = flavor.getParameter("charset");
            return new ByteArrayInputStream(
              data.getBytes(charset));
          }
          if (dataClass.equals(byte[].class)) {
            String charset = flavor.getParameter("charset");
            return data.getBytes(charset);
          }
          return data;
        }
      };

      clipboard.setContents(contents, null);

      if (action == MOVE) {
        pane.replaceSelection("");
      }
    }

    private void appendTextContent(Element element,
                                   int textStart,
                                   int textEnd,
                                   StringBuilder content)
      throws BadLocationException {
      int start = element.getStartOffset();
      int end = element.getEndOffset();
      if (end < textStart || start >= textEnd) {
        return;
      }

      start = Math.max(start, textStart);
      end = Math.min(end, textEnd);

      AttributeSet attr = element.getAttributes();
      Object tag = attr.getAttribute(AttributeSet.NameAttribute);

      if (tag.equals(HTML.Tag.HEAD) ||
        tag.equals(HTML.Tag.TITLE) ||
        tag.equals(HTML.Tag.COMMENT) ||
        tag.equals(HTML.Tag.SCRIPT)) {

        return;
      }

      if (tag.equals(HTML.Tag.INPUT) ||
        tag.equals(HTML.Tag.TEXTAREA) ||
        tag.equals(HTML.Tag.SELECT)) {

        // Swing doesn't provide a way to read input values
        // dynamically (as far as I know;  I could be wrong).
        return;
      }

      if (tag.equals(HTML.Tag.IMG)) {
        Object altText = attr.getAttribute(HTML.Attribute.ALT);
        if (altText != null) {
          content.append(altText);
        }
        return;
      }

      if (tag.equals(HTML.Tag.CONTENT)) {
        content.append(
          element.getDocument().getText(start, end - start));
        return;
      }

      int count = element.getElementCount();
      for (int i = 0; i < count; i++) {
        appendTextContent(element.getElement(i), textStart, textEnd,
          content);
      }
    }
  }




  class StyleTransferHandler extends TransferHandler {
    String mimeType = DataFlavor.javaJVMLocalObjectMimeType +
      ";class=StyledString";
    DataFlavor styledStringFlavor;

    public StyleTransferHandler() {
      try {
        styledStringFlavor = new DataFlavor(mimeType);
      }
      catch(ClassNotFoundException e) {
        System.out.println("Unable to create styledStringFlavor");
      }
    }

    public boolean canImport(JComponent comp, DataFlavor[] transferFlavors) {
      for (int j = 0; j < transferFlavors.length; j++) {
        if (styledStringFlavor.equals(transferFlavors[j]))
          return true;
      }
      return false;
    }

    protected Transferable createTransferable(JComponent c) {
      JTextPane textPane = (JTextPane)c;
      int start = textPane.getSelectionStart();
      int end = textPane.getSelectionEnd();
      StyledString ss = new StyledString("");
      if (start != -1 && start != end) {
        String text = textPane.getSelectedText();
        ss = new StyledString(text);
        StyledDocument doc = textPane.getStyledDocument();
        extractAttributes(doc, start, end, ss);
      }
      return new StyledStringTransferable(ss);
    }

    private void extractAttributes(StyledDocument doc, int selectionStart,
                                   int selectionEnd, StyledString styledStr) {
      int pos = selectionStart;
      styledStr.logicalStyle = doc.getLogicalStyle(pos);
      while (pos < selectionEnd) {
        Element element = doc.getCharacterElement(pos);
        AttributeSet attrs = element.getAttributes();
        int endOffset = element.getEndOffset();
        int end = (endOffset < selectionEnd) ? endOffset : selectionEnd;
        styledStr.addAttributes(attrs, pos, end);
        pos = end;
      }
    }

    /**
     * MOVE is not supported in superclass implementation
     * and exportDone is implemented to do nothing - see api.
     */
    public void exportAsDrag(JComponent comp, InputEvent e, int action) {
      super.exportAsDrag(comp, e, action);
      Clipboard clip = comp.getToolkit().getSystemClipboard();
      exportDone(comp, clip, action);
    }

    /**
     * MOVE is not supported in superclass implementation
     * and exportDone is implemented to do nothing - see api.
     */
    public void exportToClipboard(JComponent comp, Clipboard clip, int action) {
      super.exportToClipboard(comp, clip, action);
      exportDone(comp, clip, action);
    }

    public void exportDone(JComponent comp, Clipboard clip, int action) {
      JTextPane textPane = (JTextPane)comp;
      if (action == MOVE) {
        int offset = textPane.getSelectionStart();
        int length = textPane.getSelectionEnd() - offset;
        StyledDocument doc = textPane.getStyledDocument();
        try {
          doc.remove(offset, length);
        }
        catch (BadLocationException e) {
          System.out.printf("BadLocation error: %s%n", e.getMessage());
        }
      }
    }

    public int getSourceActions(JComponent c) {
      return COPY_OR_MOVE;
    }

    public boolean importData(JComponent comp, Transferable t) {
      if (canImport(comp, t.getTransferDataFlavors())) {
        StyledString styledStr = null;
        try {
          styledStr = (StyledString)t.getTransferData(styledStringFlavor);
          List<AttributeSet> attrs = styledStr.attrs;
          List<Location> locs = styledStr.locs;
          JTextPane textPane = (JTextPane)comp;
          int pos = textPane.getCaretPosition();
          StyledDocument doc = textPane.getStyledDocument();
          Style logicalStyle = styledStr.logicalStyle;
          // Insert the text.
          try {
            doc.insertString(pos, styledStr.text, logicalStyle);
          }
          catch (BadLocationException e) {
            System.out.printf("BadLocation error: %s%n", e.getMessage());
          }
          // Appy the style runs to the inserted text.
          for (int j = 0; j < attrs.size(); j++) {
            AttributeSet as = attrs.get(j);
            Location loc = locs.get(j);
            doc.setCharacterAttributes(pos, loc.length, as, false);
            pos += loc.length;
          }
          return true;
        }
        catch (UnsupportedFlavorException ufe) {
          System.out.println("importData UnsupportedFlavor: " +
            ufe.getMessage());
        }
        catch (IOException ioe) {
          System.out.println("importData IO Error: " + ioe.getMessage());
        }
      }
      return false;
    }

    class StyledStringTransferable implements Transferable {
      private StyledString styledString;

      StyledStringTransferable(StyledString ss) {
        styledString = ss;
      }

      public Object getTransferData(DataFlavor flavor)
        throws UnsupportedFlavorException {
        if (!isDataFlavorSupported(flavor))
          throw new UnsupportedFlavorException(flavor);
        return styledString;
      }

      public DataFlavor[] getTransferDataFlavors() {
        return new DataFlavor[] { styledStringFlavor };
      }

      public boolean isDataFlavorSupported(DataFlavor flavor) {
        return styledStringFlavor.equals(flavor);
      }

      public String toString() {
        return "StyledStringTransferable: " + styledString;
      }
    }
  }

  class StyledString {
    String text;
    List<AttributeSet> attrs;
    List<Location> locs;
    Style logicalStyle;

    public StyledString(String text) {
      this.text = text;
      attrs = new ArrayList<AttributeSet>();
      locs  = new ArrayList<Location>();
    }

    public void addAttributes(AttributeSet atts, int start, int end) {
      attrs.add(atts);
      locs.add(new Location(start, end));
    }

    public String toString() {
      StringBuilder sb = new StringBuilder("StyledString[");
      for (int j = 0; j < attrs.size(); j++) {
        sb.append("Attributes[");
        Enumeration e = attrs.get(j).getAttributeNames();
        while (e.hasMoreElements()) {
          Object key = e.nextElement();
          Object value = attrs.get(j).getAttribute(key);
          sb.append("key:" + key + ",value:" + value + ";");
        }
        sb.append("]");
        sb.append(" for " + locs.get(j));
        if (j < attrs.size()-1)
          sb.append("\n");
      }
      return sb.toString();
    }
  }


  class Location {
    int start;
    int length;

    public Location(int start, int end) {
      this.start = start;
      length = end - start;
    }

    public String toString() {
      return "Location[start:" + start + ",length:" + length + "]";
    }
  }

}


