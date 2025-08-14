package org.netbeans.api.wizard.displayer;

import java.awt.Container;
import java.awt.EventQueue;
import java.lang.reflect.InvocationTargetException;
import java.util.logging.Logger;

import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.border.EmptyBorder;

import org.netbeans.api.wizard.WizardDisplayer;
import org.netbeans.modules.wizard.NbBridge;
import org.netbeans.spi.wizard.ResultProgressHandle;
import org.netbeans.spi.wizard.Summary;

/**
 * Show progress bar for deferred results, with a label showing percent done and progress bar.
 * 
 * <p>
 * <b><i><font color="red">This class is NOT AN API CLASS.  There is no
 * commitment that it will remain backward compatible or even exist in the
 * future.  The API of this library is in the packages <code>org.netbeans.api.wizard</code>
 * and <code>org.netbeans.spi.wizard</code></font></i></b>.

 * @author stanley@stanleyknutson.com
 * @author Kevin A. Archie <karchie@wustl.edu>
 */
public class NavProgress implements ResultProgressHandle
{
    private static final Logger logger = Logger.getLogger(NavProgress.class.getName());
    private static final Icon busyIcon =  new ImageIcon(NavProgress.class.getResource("busy.gif"));
    private static final String MESSAGE_SPACE = // 64 underscores
        "________________________________________________________________";
    
    private final JPanel panel = new JPanel();
    private final JProgressBar progressBar = new JProgressBar();
    private final JLabel messageLabel = new JLabel(MESSAGE_SPACE);
    private final JLabel busyLabel = new JLabel();

    private final WizardDisplayerImpl parent;

    private final boolean disableUIWhileBusy;
    
    /** isRunning is true until finished or failed is called */
    private boolean isRunning = true;
    
    NavProgress(final WizardDisplayerImpl impl, final boolean disableUIWhileBusy) {
        this.parent = impl;
        this.disableUIWhileBusy = disableUIWhileBusy;
        panel.setOpaque(false);
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.add(messageLabel);
        if (disableUIWhileBusy) {
            panel.add(busyLabel);
        } else {
            panel.add(progressBar);
        }
        setBusy();
    }
    
    public void addProgressComponents(final Container container) {
        container.add(panel);
        container.invalidate();
    }

    public void setProgress(final String description, final int currentStep, final int totalSteps)
    {
        Runnable r = new Runnable()
        {
            public void run()
            {
                messageLabel.setText(description == null ? " " : description); // NOI18N
                messageLabel.revalidate();
                setProgress(currentStep, totalSteps);
                panel.repaint();
            }
        };
        invoke(r);
    }

    public void setProgress(final int currentStep, final int totalSteps) {
        if (totalSteps == -1) {
            setBusy();
        } else {
            if (currentStep > totalSteps || currentStep < 0) {
                throw new IllegalArgumentException("Bad step values: " // NOI18N
                        + currentStep + " out of " + totalSteps); // NOI18N
            }

            invoke(new Runnable() {
                public void run() {
                    if (disableUIWhileBusy) {
                        busyLabel.setIcon(null);
                    } else {
                        progressBar.setIndeterminate(false);
                        progressBar.setMaximum(totalSteps);
                        progressBar.setValue(currentStep);
                    }
                    panel.repaint();
                }
            });
        }
    }

    private void setBusy() {
        invoke(new Runnable() {
            public void run() {
                if (disableUIWhileBusy) {
                    busyLabel.setIcon(busyIcon);
                } else {
                    progressBar.setIndeterminate(true);
                }
            }
        });
    }
    
    public void setBusy (final String description) {
        invoke(new Runnable() {
            public void run() {
                messageLabel.setText(description == null ? " " : description); // NOI18N
                setBusy();
            }
        });
    }
        
    private void invoke(Runnable r)
    {
        if (EventQueue.isDispatchThread())
        {
            r.run();
        }
        else
        {
            try
            {
                EventQueue.invokeAndWait(r);
            }
            catch (InvocationTargetException ex)
            {
                ex.printStackTrace();
                logger.severe("Error invoking operation " + ex.getClass().getName() + " " + ex.getMessage());
            }
            catch (InterruptedException ex)
            {
                logger.severe("Error invoking operation " + ex.getClass().getName() + " " + ex.getMessage());
                ex.printStackTrace();
            }
        }
    }

    public void finished(final Object o)
    {
        isRunning = false;
        Runnable r = new Runnable()
        {
            public void run()
            {
                if (o instanceof Summary)
                {
                    Summary summary = (Summary) o;
                    parent.handleSummary(summary);
                    parent.setWizardResult(summary.getResult());
                }
                else if (parent.getDeferredResult() != null)
                {
                    parent.setWizardResult(o);

                    // handle result based on which button was pushed
                    parent.getButtonManager().deferredResultFinished(o);
                }
            }
        };
        invoke(r);
    }

    public void failed(final String message, final boolean canGoBack)
    {
        isRunning = false;

        Runnable r = new Runnable()
        {
            public void run()
            {
                // cheap word wrap
                JLabel comp = new JLabel("<html><body>" + message + "</body></html>"); // NOI18N
                comp.setBorder(new EmptyBorder(5, 5, 5, 5));
                parent.setCurrentWizardPanel(comp);
                parent.getTtlLabel().setText(
                                             NbBridge
                                                 .getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                                            WizardDisplayer.class, "Failed")); // NOI18N
                NavButtonManager bm = parent.getButtonManager();
                bm.deferredResultFailed(canGoBack);
            }
        };
        invoke(r);
    }

    public boolean isRunning()
    {
        return isRunning;
    }
}
