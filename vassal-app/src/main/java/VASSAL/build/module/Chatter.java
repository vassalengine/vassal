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

import VASSAL.tools.ProblemDialog;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.net.URL;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.FontConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.ScrollPane;

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
  protected static final String MY_CHAT_COLOR    = "HTMLChatColor";          //$NON-NLS-1$ // Different tags to "restart" w/ new default scheme
  protected static final String OTHER_CHAT_COLOR = "HTMLotherChatColor";     //$NON-NLS-1$
  protected static final String GAME_MSG1_COLOR  = "HTMLgameMessage1Color";  //$NON-NLS-1$
  protected static final String GAME_MSG2_COLOR  = "HTMLgameMessage2Color";  //$NON-NLS-1$ 
  protected static final String GAME_MSG3_COLOR  = "HTMLgameMessage3Color";  //$NON-NLS-1$ 
  protected static final String GAME_MSG4_COLOR  = "HTMLgameMessage4Color";  //$NON-NLS-1$ 
  protected static final String GAME_MSG5_COLOR  = "HTMLgameMessage5Color";  //$NON-NLS-1$ 
  protected static final String SYS_MSG_COLOR    = "HTMLsystemMessageColor"; //$NON-NLS-1$

  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected static final String GAME_MSG_COLOR = "gameMessageColor";

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
    conversationPane.setContentType("text/html");
    doc = (HTMLDocument) conversationPane.getDocument();
    kit = (HTMLEditorKit) conversationPane.getEditorKit();

    style = kit.getStyleSheet();
    myFont = new Font("SansSerif", Font.PLAIN, 12); // Will be overridden by the font from Chat preferences

    try {
      for (int i = 0; i < 15; ++i) {
        kit.insertHTML(doc, doc.getLength(), "<br>", 0, 0, null);
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
      send(formatChat(e.getActionCommand()));
      input.setText(""); //$NON-NLS-1$
    });
    input.setMaximumSize(new Dimension(input.getMaximumSize().width, input.getPreferredSize().height));
    
    FontMetrics fm = getFontMetrics(myFont);
    int fontHeight = fm.getHeight();

    conversationPane.setPreferredSize(new Dimension(input.getMaximumSize().width, fontHeight * 10));

    scroll.setViewportView(conversationPane);
    scroll.getVerticalScrollBar().setUnitIncrement(input.getPreferredSize().height); //Scroll this faster
    add(scroll);
    add(input);
    
    setPreferredSize(new Dimension(input.getMaximumSize().width, input.getPreferredSize().height + conversationPane.getPreferredSize().height));
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
   * 
   * To make the player colors easy to override in a custom class
   * (my modules have logic to assign individual player colors -- beyond the scope of the present effort but a perhaps a fun future addition)
   * @param s - chat message from a player
   * @return - an entry in our CSS style sheet to use for this chat message
   */
  protected String getChatStyle(String s) {   
    return s.startsWith(formatChat("").trim()) ? "mychat" : "other";
  }
    
  /**
   * A hook for inserting a console class that accepts commands
   * @param s            - chat message
   * @param style        - current style name (contains information that might be useful)
   * @param html_allowed - flag if html_processing is enabled for this message (allows console to apply security considerations)
   * @return true        - if was accepted as a console command
   */
  @SuppressWarnings("unused")
  public boolean consoleHook(String s, String style, boolean html_allowed) {
    return false;
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
    String style;
    boolean html_allowed;

    // Choose an appropriate style to display this message in
    s = s.trim();
    if (!s.isEmpty()) {
      if (s.startsWith("*")) {
        // Here we just extend the convention of looking at first characters, this time to the second character.
        // | = msg1 (w/ HTML parsing explicitly opted in)
        // ! = msg2
        // ? = msg3
        // ~ = msg4
        // ` = msg5
        // These characters can be pre-pended to Report messages to produce the color changes. The characters themselves are removed before display.
        // Reports can also include <b></b> tags for bold and <i></i> for italic.
        if (s.startsWith("* |") || s.startsWith("*|")) {
          style = "msg";
          s = s.replaceFirst("\\|", "");
          html_allowed = true;
        } 
        else if (s.startsWith("* !") || s.startsWith("*!")) {
          style = "msg2";
          s = s.replaceFirst("!", "");
          html_allowed = true;
        } 
        else if (s.startsWith("* ?") || s.startsWith("*?")) {
          style = "msg3";
          s = s.replaceFirst("\\?", "");
          html_allowed = true;
        } 
        else if (s.startsWith("* ~") || s.startsWith("*~")) {
          style = "msg4";
          s = s.replaceFirst("~", "");
          html_allowed = true;
        } 
        else if (s.startsWith("* `") || s.startsWith("*`")) {
          style = "msg5";
          s = s.replaceFirst("`", "");
          html_allowed = true;
        } 
        else {
          style = "msg";
          html_allowed = GlobalOptions.getInstance().chatterHTMLSupport(); // Generic report lines check compatibility flag (so old modules will not break on e.g. "<" in messages)
        }
      } 
      else if (s.startsWith("-")) {
        style = "sys";
        html_allowed = true;
      } 
      else {
        style = getChatStyle(s);
        html_allowed = false;
      }
    } 
    else {
      style = "msg";
      html_allowed = false;
    }

    // Disable unwanted HTML tags in contexts where it shouldn't be allowed:
    // (1) Anything received from chat channel, for security reasons
    // (2) Legacy module "report" text when not explicitly opted in w/ first character or preference setting 
    if (!html_allowed) {
      s = s.replaceAll("<", "&lt;")  // This prevents any unwanted tag from functioning
           .replaceAll(">", "&gt;"); // This makes sure > doesn't break any of our legit <div> tags
    }

    // Systematically search through for html image tags. When we find one, try
    // to match it with an image from our DataArchive, and substitute the correct
    // fully qualified URL into the tag.
    URL url;
    String keystring = "<img src=\"";
    String file, tag, replace;
    int base;
    while (s.toLowerCase().contains(keystring)) { // Find next key (to-lower so we're not case sensitive)
      base = s.toLowerCase().indexOf(keystring);
      file = s.substring(base + keystring.length()).split("\"")[0]; // Pull the filename out from between the quotes
      tag  = s.substring(base, base + keystring.length()) + file + "\""; // Reconstruct the part of the tag we want to remove, leaving all attributes after the filename alone, and properly matching the upper/lowercase of the keystring

      try {
        url = GameModule.getGameModule().getDataArchive().getURL("images/" + file);
        replace = "<img  src=\"" + url.toString() + "\""; // Fully qualified URL if we are successful. The extra
                                                          // space between IMG and SRC in the processed
                                                          // version ensures we don't re-find THIS tag as we iterate
      } 
      catch (IOException ex) {
        replace = "<img  src=\"" + file + "\""; // Or just leave in except alter just enough that we won't find this tag again
      }

      if (s.contains(tag)) {
        s = s.replaceFirst(tag, replace); // Swap in our new URL-laden tag for the old one.
      } 
      else {
        break; // If something went wrong in matching up the tag, don't loop forever
      }
    }

    // Now we have to fix up any legacy angle brackets around the word <observer>
    keystring = Resources.getString("PlayerRoster.observer");
    replace = keystring.replace("<", "&lt;").replace(">", "&gt;");
    if (!replace.equals(keystring)) {
      s = s.replace(keystring, replace);
    }

    // Insert a div of the correct style for our line of text. Module designer
    // still free to insert <span> tags and <img> tags and the like in Report
    // messages.
    try {
      kit.insertHTML(doc, doc.getLength(), "\n<div class=" + style + ">" + s + "</div>", 0, 0, null);
    } 
    catch (BadLocationException | IOException ble) {
      ErrorDialog.bug(ble);
    }

    consoleHook(s, style, html_allowed);
  }

  /**
   * Adds or updates a CSS stylesheet entry. Styles in the color, font type, and font size.
   * @param s Style name
   * @param f Font to use
   * @param c Color for text
   * @param font_weight Bold? Italic?
   * @param size Font size
   */
  protected void addStyle(String s, Font f, Color c, String font_weight, int size) {
    if ((style == null) || (c == null)) return;
    style.addRule(s + 
                  " {color:" + 
                  String.format("#%02x%02x%02x", c.getRed(), c.getGreen(), c.getBlue()) + 
                  "; font-family:" + 
                  f.getFamily() + 
                  "; font-size:" + 
                  (size > 0 ? size : f.getSize()) + 
                  "; " + 
                  ((!font_weight.isBlank()) ? "font-weight:" + font_weight + "; " : "") +
                  "}");
  }

  /**
   * Build ourselves a CSS stylesheet from our preference font/color settings.
   * @param f - Font to use for this stylesheet
   */
  protected void makeStyleSheet(Font f) {
    if (style == null) {
      return;
    }

    if (f == null) {
      if (myFont == null) {
        f = new Font("SansSerif", Font.PLAIN, 12);
        myFont = f;
      } 
      else {
        f = myFont;
      }
    }
    addStyle(".msg",    f, gameMsg,   "",     0);
    addStyle(".msg2",   f, gameMsg2,  "",     0);
    addStyle(".msg3",   f, gameMsg3,  "",     0);
    addStyle(".msg4",   f, gameMsg4,  "",     0);
    addStyle(".msg5",   f, gameMsg5,  "",     0);
    addStyle(".mychat", f, myChat,    "bold", 0);
    addStyle(".other ", f, otherChat, "bold", 0);
    addStyle(".sys",    f, systemMsg, "",     0);

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
    GameModule mod = (GameModule) b;
    mod.setChatter(this);
    mod.addCommandEncoder(this);
    mod.addKeyStrokeSource(new KeyStrokeSource(this, WHEN_ANCESTOR_OF_FOCUSED_COMPONENT));

    final FontConfigurer chatFont = new FontConfigurer("ChatFont",
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


    final ColorConfigurer otherChatColor = new ColorConfigurer(OTHER_CHAT_COLOR,
                                                                Resources.getString("Chatter.other_text_preference"), new Color (0, 153, 255));

    otherChatColor.addPropertyChangeListener(e -> {
      otherChat = (Color) e.getNewValue();
      makeStyleSheet(null);
    });

    globalPrefs.addOption(Resources.getString("Chatter.chat_window"), otherChatColor);
    otherChat = (Color) globalPrefs.getValue(OTHER_CHAT_COLOR);
    
    // Put up the HTML Enable/Disable checkbox if we're supposed to have it.
    if (GlobalOptions.PROMPT.equals(GlobalOptions.getInstance().chatterHTMLSetting())) {
      BooleanConfigurer config2 = new BooleanConfigurer(GlobalOptions.CHATTER_HTML_SUPPORT, Resources.getString("GlobalOptions.chatter_html_support")); //$NON-NLS-1$
      globalPrefs.addOption(Resources.getString("Chatter.chat_window"), config2);        
    }

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
   * Warning message method -- same as send, but accepts messages from static methods. For reporting soft-fail problems in modules.  
   */
  public static void warning(String msg) {
    Chatter chatter = GameModule.getGameModule().getChatter();
    chatter.send(msg);
  }
  

  /**
   * Classes other than the Chatter itself may forward KeyEvents to the Chatter by
   * using this method
   */
  public void keyCommand(KeyStroke e) {
    if ((e.getKeyCode() == 0 || e.getKeyCode() == KeyEvent.CHAR_UNDEFINED)
        && !Character.isISOControl(e.getKeyChar())) {
      input.setText(input.getText() + e.getKeyChar());
    } 
    else if (e.isOnKeyRelease()) {
      switch (e.getKeyCode()) {
      case KeyEvent.VK_ENTER:
        if (!input.getText().isEmpty())
          send(formatChat(input.getText()));
        input.setText(""); //$NON-NLS-1$
        break;
      case KeyEvent.VK_BACK_SPACE:
      case KeyEvent.VK_DELETE:
        String s = input.getText();
        if (!s.isEmpty())
          input.setText(s.substring(0, s.length() - 1));
        break;
      }
    }
  }


  /**
   * This is a {@link Command} object that, when executed, displays
   * a text message in the Chatter's text area     */
  public static class DisplayText extends Command {

    public static final String PREFIX = "CHAT";  //$NON-NLS-1$

    private String msg;
    private final Chatter c;

    public DisplayText(Chatter c, String s) {
      this.c = c;
      msg = s;
      if (msg.startsWith("<>")) {
        msg = "&lt;(" + Chatter.getAnonymousUserName() + ")&gt;" + s.substring(2); // HTML-friendly
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
    Chatter chat = new Chatter();
    JFrame f = new JFrame();
    f.add(chat);
    f.pack();
    f.setVisible(true);
  }

  /** @deprecated No Replacement */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void setHandle(@SuppressWarnings("unused") String s) {
    ProblemDialog.showDeprecated("2020-08-06");
  }

  /** @deprecated use {@link GlobalOptions#getPlayerId()} */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public String getHandle() {
    ProblemDialog.showDeprecated("2020-08-06");
    return GlobalOptions.getInstance().getPlayerId();
  }
}
