package VASSAL.tools.swing;

import java.awt.BorderLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.UIManager;

import java.awt.Component;

import javax.swing.JSplitPane;

import org.apache.commons.lang3.SystemUtils;

public class SplitPane extends JSplitPane {
  private static final long serialVersionUID = 1L;

  private int prevDividerLocation = 0;
  private int prevDividerSize = 0;

  public SplitPane(int orientation, Component first, Component second) {
    super(orientation, first, second);
    setDividerLocation(0.5);
    setContinuousLayout(true);
  }

  public void setLeftVisible(boolean vis) {
    setComponentVisible(leftComponent, vis);
  }

  public void setRightVisible(boolean vis) {
    setComponentVisible(rightComponent, vis);
  }

  public void setTopVisible(boolean vis) {
    setComponentVisible(leftComponent, vis);
  }

  public void setBottomVisible(boolean vis) {
    setComponentVisible(rightComponent, vis);
  }

  private void setComponentVisible(Component c, boolean vis) {
    if (c != null && c.isVisible() != vis) {
      toggleComponent(c);
    }
  }

  public boolean isLeftVisible() {
    return leftComponent != null && leftComponent.isVisible();
  }

  public boolean isRightVisible() {
    return rightComponent != null && rightComponent.isVisible();
  }

  public boolean isTopVisible() {
    return leftComponent != null && leftComponent.isVisible();
  }

  public boolean isBottomVisible() {
    return rightComponent != null && rightComponent.isVisible();
  }

  public void showLeft() {
    showComponent(leftComponent);
  }

  public void showRight() {
    showComponent(rightComponent);
  }

  public void showTop() {
    showComponent(leftComponent);
  }

  public void showBottom() {
    showComponent(rightComponent);
  }

  private void showComponent(Component c) {
    if (c != null && !c.isVisible()) {
      toggleComponent(c);
    }
  }

  public void hideLeft() {
    hideComponent(leftComponent);
  }

  public void hideRight() {
    hideComponent(rightComponent);
  }

  public void hideTop() {
    hideComponent(leftComponent);
  }

  public void hideBottom() {
    hideComponent(rightComponent);
  }

  private void hideComponent(Component c) {
    if (c != null && c.isVisible()) {
      toggleComponent(c);
    }
  }

  public void toggleLeft() {
    toggleComponent(leftComponent);
  }

  public void toggleRight() {
    toggleComponent(rightComponent);
  }

  public void toggleTop() {
    toggleComponent(leftComponent);
  }

  public void toggleBottom() {
    toggleComponent(rightComponent);
  }

  private void toggleComponent(Component c) {
    if (c != null) {
      c.setVisible(!c.isVisible());

      int tmp = getDividerSize();
      setDividerSize(prevDividerSize);
      prevDividerSize = tmp;

      tmp = getDividerLocation();
      setDividerLocation(prevDividerLocation);
      prevDividerLocation = tmp;
    }
  }

  public static void main(String[] args) throws Exception {
    if (!SystemUtils.IS_OS_WINDOWS) {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
    }

    final JButton left = new JButton("Left");
    final JButton right = new JButton("Right");
    final SplitPane sp = new SplitPane(HORIZONTAL_SPLIT, left, right);

    left.addActionListener(e -> {
      if (left.isVisible() && right.isVisible()) {
        sp.toggleLeft();
      }
      else {
        sp.toggleRight();
      }
    });
    right.addActionListener(e -> {
      if (left.isVisible() && right.isVisible()) {
        sp.toggleRight();
      }
      else {
        sp.toggleLeft();
      }
    });

    final JFrame f = new JFrame();
    f.add(sp, BorderLayout.CENTER);
    f.setSize(400, 300);
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    f.setLocationRelativeTo(null);
    f.setVisible(true);
  }
}
