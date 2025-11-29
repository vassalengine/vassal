package VASSAL.build.module.videoexport;

import VASSAL.i18n.Resources;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;

/**
 * Swing dialog layout for video export. Exposes setters and buttons for wiring in logic.
 */
class VideoExportDialog {
  private final JFrame frame;
  private final JTextField logField;
  private final JButton browseLogs;
  private final JTextField outField;
  private final JButton browseOut;
  private final JButton cropButton;
  private final JButton fullMapButton;
  private final JButton startButton;
  private final JLabel cropStatus;
  private final JTextField fpsField;
  private final JTextField widthField;
  private final JTextField heightField;

  VideoExportDialog() {
    frame = new JFrame(Resources.getString("VideoExporter.button"));
    frame.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

    logField = new JTextField();
    logField.setEditable(false);
    browseLogs = new JButton("Select Log Folder");
    outField = new JTextField();
    outField.setEditable(false);
    browseOut = new JButton("Select Output");
    cropButton = new JButton("Select Crop Area");
    cropButton.setEnabled(false);
    fullMapButton = new JButton("Use Full Map");
    fullMapButton.setEnabled(false);
    startButton = new JButton("Start Rendering");
    startButton.setEnabled(false);
    cropStatus = new JLabel("Crop: full-map");
    cropStatus.setAlignmentX(Component.LEFT_ALIGNMENT);
    fpsField = new JTextField();
    widthField = new JTextField();
    heightField = new JTextField();

    final JPanel content = new JPanel();
    content.setLayout(new BoxLayout(content, BoxLayout.Y_AXIS));

    final JPanel logsPanel = new JPanel(new BorderLayout(5, 5));
    logsPanel.add(new JLabel("Log folder:"), BorderLayout.WEST);
    logsPanel.add(logField, BorderLayout.CENTER);
    logsPanel.add(browseLogs, BorderLayout.EAST);

    final JPanel outPanel = new JPanel(new BorderLayout(5, 5));
    outPanel.add(new JLabel("Output file:"), BorderLayout.WEST);
    outPanel.add(outField, BorderLayout.CENTER);
    outPanel.add(browseOut, BorderLayout.EAST);

    final JPanel fpsPanel = new JPanel(new BorderLayout(5, 5));
    fpsPanel.add(new JLabel("FPS:"), BorderLayout.WEST);
    fpsPanel.add(fpsField, BorderLayout.CENTER);

    final JPanel resPanel = new JPanel(new BorderLayout(5, 5));
    resPanel.add(new JLabel("Max resolution (W x H):"), BorderLayout.WEST);
    final JPanel resFields = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
    resFields.add(widthField);
    resFields.add(new JLabel("x"));
    resFields.add(heightField);
    resPanel.add(resFields, BorderLayout.CENTER);

    final JPanel buttons = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    buttons.add(cropButton);
    buttons.add(fullMapButton);
    buttons.add(startButton);

    final JLabel heading = new JLabel("Render Logs to Video");
    heading.setAlignmentX(Component.LEFT_ALIGNMENT);
    final JLabel subText = new JLabel("Select a log folder, optional crop area, and output mp4.");
    subText.setAlignmentX(Component.LEFT_ALIGNMENT);

    content.add(heading);
    content.add(subText);
    content.add(Box.createVerticalStrut(8));
    content.add(logsPanel);
    content.add(Box.createVerticalStrut(8));
    content.add(outPanel);
    content.add(Box.createVerticalStrut(8));
    content.add(fpsPanel);
    content.add(Box.createVerticalStrut(8));
    content.add(resPanel);
    content.add(Box.createVerticalStrut(8));
    final JPanel cropStatusPanel = new JPanel(new BorderLayout());
    cropStatusPanel.add(cropStatus, BorderLayout.CENTER);
    content.add(cropStatusPanel);
    content.add(Box.createVerticalStrut(8));
    content.add(buttons);

    frame.getContentPane().add(content);
    frame.pack();
  }

  void show() {
    frame.setVisible(true);
  }

  void dispose() {
    frame.dispose();
  }

  JFileChooser newLogChooser() {
    final JFileChooser dirChooser = new JFileChooser();
    dirChooser.setDialogTitle(Resources.getString("VideoExporter.folder_dialog"));
    dirChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    return dirChooser;
  }

  JFileChooser newOutputChooser() {
    final JFileChooser outChooser = new JFileChooser();
    outChooser.setDialogTitle(Resources.getString("VideoExporter.output_dialog"));
    return outChooser;
  }

  void setLogPath(String path) {
    logField.setText(path);
  }

  void setOutputPath(String path) {
    outField.setText(path);
  }

  void setCropStatus(String status) {
    cropStatus.setText(status);
  }

  void setCropButtonText(String text) {
    cropButton.setText(text);
  }

  void setEnabledStates(boolean browsingEnabled, boolean cropEnabled, boolean fullMapEnabled, boolean startEnabled) {
    browseLogs.setEnabled(browsingEnabled);
    browseOut.setEnabled(browsingEnabled);
    fpsField.setEnabled(browsingEnabled);
    widthField.setEnabled(browsingEnabled);
    heightField.setEnabled(browsingEnabled);
    cropButton.setEnabled(cropEnabled);
    fullMapButton.setEnabled(fullMapEnabled);
    startButton.setEnabled(startEnabled);
  }

  JButton getBrowseLogs() {
    return browseLogs;
  }

  JButton getBrowseOut() {
    return browseOut;
  }

  JButton getCropButton() {
    return cropButton;
  }

  JButton getFullMapButton() {
    return fullMapButton;
  }

  JButton getStartButton() {
    return startButton;
  }

  void setFps(int fps) {
    fpsField.setText(Integer.toString(fps));
  }

  String getFpsText() {
    return fpsField.getText();
  }

  void setResolution(int width, int height) {
    widthField.setText(Integer.toString(width));
    heightField.setText(Integer.toString(height));
  }

  String getWidthText() {
    return widthField.getText();
  }

  String getHeightText() {
    return heightField.getText();
  }

  JFrame getFrame() {
    return frame;
  }
}
