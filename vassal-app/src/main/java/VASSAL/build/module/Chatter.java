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

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.FontConfigurer;
import VASSAL.configure.StructuredFontConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.QuickColors;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.swing.DataArchiveHTMLEditorKit;
import VASSAL.tools.swing.SwingUtils;

import org.apache.commons.io.FileUtils;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;
import javax.swing.undo.UndoManager;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

/**
 * The chat window component. Displays text messages and accepts input. Also
 * acts as a {@link CommandEncoder}, encoding/decoding commands that display
 * message in the text area
 */
public class Chatter extends JPanel implements CommandEncoder, Buildable, DropTargetListener {
  private static final long serialVersionUID = 1L;

  protected JTextPane conversationPane;
  protected HTMLDocument doc;
  protected HTMLEditorKit kit;
  protected StyleSheet style;

  protected JTextField input;
  protected JScrollPane scroll = new ScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
  protected JScrollPane scroll2 = new ScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
  protected FontConfigurer oldFontConfig;
  protected StructuredFontConfigurer fontConfig;

  public static final String FONT = "ChatFont";
  public static final String FONT2 = "ChatFont2";     // New Pref key for Chat Font to prevent crashing older modules
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

  protected DropTarget dt;

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
    conversationPane.getCaret().setVisible(false);
    conversationPane.setCaretColor(new Color(0, 0, 0, 0));

    conversationPane.addComponentListener(new ComponentAdapter() {
      @Override
      public void componentResized(ComponentEvent e) {
        scroll.getVerticalScrollBar().setValue(scroll.getVerticalScrollBar().getMaximum());
      }
    });

    input = new JTextField(60);
    input.setFocusTraversalKeysEnabled(false);
    input.setMaximumSize(new Dimension(input.getMaximumSize().width, input.getPreferredSize().height));

    input.addKeyListener(new KeyListener() {
      @Override
      public void keyTyped(KeyEvent e) {
      }

      @Override
      public void keyPressed(KeyEvent e) {
      }

      @Override
      public void keyReleased(KeyEvent e) {
        final int code = e.getKeyCode();
        final Console console = GameModule.getGameModule().getConsole();
        String newCommand = null;
        switch (code) {
        case KeyEvent.VK_UP:
          newCommand = console.commandsUp();
          break;
        case KeyEvent.VK_DOWN:
          newCommand = console.commandsDown();
          break;
        case KeyEvent.VK_PAGE_UP:
        case KeyEvent.VK_HOME:
          newCommand = console.commandsTop();
          break;
        case KeyEvent.VK_PAGE_DOWN:
        case KeyEvent.VK_END:
          newCommand = console.commandsBottom();
          break;
        }
        if (newCommand != null) {
          input.setText(newCommand);
        }
      }
    });

    final UndoManager um = SwingUtils.allowUndo(input);

    input.addActionListener(e -> {
      send(formatChat(e.getActionCommand()), e.getActionCommand());
      input.setText(""); //$NON-NLS-1$
      um.discardAllEdits();
    });

    scroll.setViewportView(conversationPane);
    scroll.getVerticalScrollBar().setUnitIncrement(input.getPreferredSize().height); //Scroll this faster

    final int fontHeight = getFontMetrics(myFont).getHeight();
    setPreferredSize(new Dimension(
      input.getMaximumSize().width,
      input.getPreferredSize().height + 10 * fontHeight
    ));

    add(scroll);
    add(input);

    final Action copyAction = new DefaultEditorKit.CopyAction();
    copyAction.putValue(Action.NAME, Resources.getString("Chat.copy"));

    final Action saveAction = new AbstractAction(Resources.getString("Chat.save_to_html_file")) {
      @Override
      public void actionPerformed(ActionEvent e) {
        final FileChooser fc = GameModule.getGameModule().getFileChooser();

        if (fc.showSaveDialog(GameModule.getGameModule().getControlPanel()) != FileChooser.APPROVE_OPTION) {
          return;
        }

        final File sf = fc.getSelectedFile();
        String name = sf.getPath();
        final int index = name.lastIndexOf('.');
        if (index < 0) {
          name = name + ".html";
          fc.setSelectedFile(new File(name));
        }

        final File outputFile = fc.getSelectedFile();
        if (outputFile == null) {
          return;
        }

        try (OutputStream fos = Files.newOutputStream(outputFile.toPath())) {
          kit.write(fos, doc, 0, doc.getLength());
        }
        catch (IOException | BadLocationException ex) {
          GameModule.getGameModule().warn(Resources.getString("Chat.file_save_failed"));
        }
      }
    };

    final Action saveTextAction = new AbstractAction(Resources.getString("Chat.save_to_text_file")) {
      @Override
      public void actionPerformed(ActionEvent e) {
        final FileChooser fc = GameModule.getGameModule().getFileChooser();

        if (fc.showSaveDialog(GameModule.getGameModule().getControlPanel()) != FileChooser.APPROVE_OPTION) {
          return;
        }

        final File sf = fc.getSelectedFile();
        String name = sf.getPath();
        final int index = name.lastIndexOf('.');
        if (index < 0) {
          name = name + ".txt";
          fc.setSelectedFile(new File(name));
        }

        final File outputFile = fc.getSelectedFile();
        if (outputFile == null) {
          return;
        }

        try {
          FileUtils.writeStringToFile(outputFile, doc.getText(0, doc.getLength()), StandardCharsets.UTF_8); //NON-NLS
        }
        catch (IOException | BadLocationException ex) {
          GameModule.getGameModule().warn(Resources.getString("Chat.file_save_failed"));
        }
      }
    };

    final JPopupMenu pm = new JPopupMenu();
    pm.add(copyAction);
    pm.add(saveAction);
    pm.add(saveTextAction);

    conversationPane.addMouseListener(new MouseAdapter() {
      @Override
      public void mousePressed(MouseEvent e) {
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

    final Action cutAction = new DefaultEditorKit.CutAction();
    cutAction.putValue(Action.NAME, Resources.getString("General.cut"));

    final Action pasteAction = new DefaultEditorKit.PasteAction();
    pasteAction.putValue(Action.NAME, Resources.getString("General.paste"));

    final JPopupMenu pm2 = new JPopupMenu();
    pm2.add(cutAction);
    pm2.add(copyAction);
    pm2.add(pasteAction);

    input.addMouseListener(new MouseAdapter() {
      @Override
      public void mousePressed(MouseEvent e) {
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
        pm2.show(e.getComponent(), e.getX(), e.getY());
      }
    });

    // Accept dropped files
    dt = new DropTarget(conversationPane, this);
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
      kit.insertHTML(doc, doc.getLength(), "\n<div class=\"" + style + "\">" + s + "</div>", 0, 0, null); //NON-NLS
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

    //
    // The old FontConfigurer only stored the Font and size, no style, The new StructuredFontConfigurer stores Font, Style and Size.
    // If we swap one straight for the other, it causes the module to crash and fail to open if opened with an earlier version
    // of Vassal and can't be fixed until the Style is manually removed from the preference file.
    // So, the conversion process is as follows:
    //  1. Keep old pref with key "fontConfig"
    //  2. Mew config will use key "fontConfig2"
    //  3. Create an invisible pref for the old key and see if it has a value that is different from the default value
    //  4. Create a standard pref for the new key and see if it has a value that is different from the default value.
    //  5. If the old pref has a non-default value AND the new pref has a default value, then copy the value to the new pref
    //
    oldFontConfig = new FontConfigurer(FONT, Resources.getString("Chatter.chat_font_preference"));
    final Font oldDefaultFont = (Font) oldFontConfig.getValue();
    final Configurer c = mod.getPrefs().getOption(FONT);
    mod.getPrefs().addOption(null, oldFontConfig);
    final Font oldPrefFont = (Font) oldFontConfig.getValue();

    final Configurer c2 = mod.getPrefs().getOption(FONT);

    fontConfig = new StructuredFontConfigurer(FONT2, Resources.getString("Chatter.chat_font_preference"));
    fontConfig.setModuleSpecific(false);
    fontConfig.setPlainOnly(true);
    fontConfig.setLimitedSizes(true);
    fontConfig.addPropertyChangeListener(evt -> {
      setFont((Font) evt.getNewValue());
      oldFontConfig.setValue(myFont);
    });

    mod.getPlayerWindow().addChatter(this);
    final Font newPrefFont = fontConfig.getValueFont();

    fontConfig.fireUpdate();
    mod.getPrefs().addOption(Resources.getString("Chatter.chat_window"), fontConfig); //$NON-NLS-1$
    final Font newDefaultFont = fontConfig.getValueFont();

    // Copy old pref value to new pref if old pref has been updated from default, but new font is still default
    if (!oldPrefFont.equals(oldDefaultFont) && newPrefFont.equals(newDefaultFont)) {
      fontConfig.setValue(new Font(oldPrefFont.getFontName(), Font.PLAIN, oldPrefFont.getSize()));
    }

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
        if (!input.getText().isEmpty()) {
          send(formatChat(input.getText()), input.getText());
        }
        input.setText(""); //$NON-NLS-1$
        break;
      case KeyEvent.VK_BACK_SPACE:
      case KeyEvent.VK_DELETE:
        final String s = input.getText();
        if (!s.isEmpty()) {
          input.setText(s.substring(0, s.length() - 1));
        }
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

    private final String msg;
    private final Chatter c;

    public DisplayText(Chatter c, String s) {
      this.c = c;
      if (s.startsWith("<>")) { //NON-NLS
        msg = "&lt;(" + getAnonymousUserName() + ")&gt;" + s.substring(2); // HTML-friendly //NON-NLS
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



  @Override
  public void dragEnter(DropTargetDragEvent event) {
  }

  @Override
  public void dragExit(DropTargetEvent event) {
  }

  @Override
  public void dragOver(DropTargetDragEvent event) {
  }

  @Override
  public void dropActionChanged(DropTargetDragEvent event) {
  }

  /**
   * We put the "drop" in drag-n-drop!
   * @param dtde DropTargetDragEvent
   */
  @Override
  public void drop(DropTargetDropEvent dtde) {
    GameModule.getGameModule().getGameState().dropFile(dtde);
  }

  @Override
  public boolean isMandatory() {
    return true;
  }

  @Override
  public boolean isUnique() {
    return true;
  }
}


