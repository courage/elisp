;;----------------------------------------------------------------------
;; Functions for making nice balanced columns in a frame.

(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

(defun split-into-two-columns ()
  "Split the frame into two columns"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally))

(defun split-into-two-columns-and-cycle ()
  "Split the frame into two columns and select the next buffer in the second window."
  (interactive)
  (split-into-two-columns)
  (select-next-window)
  (next-buffer)
  (select-next-window))

(defun split-into-two-columns-and-follow ()
  "Split the frame into two columns and enable follow mode."
  (interactive)
  (split-into-two-columns)
  (follow-mode 1))

(defun split-into-three-columns ()
  "Split the frame into three columns"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(defun split-into-three-columns-and-cycle ()
  "Split the frame into three columns and select new buffers in the other windows. The current window
will be in the middle of the new layout."
  (interactive)
  (split-into-three-columns)
  (next-buffer)
  (select-next-window)
  (select-next-window)
  (next-buffer)
  (next-buffer)
  (select-next-window)
  (select-next-window))

(defun split-into-three-columns-and-follow ()
  "Split the frame into three columns and turn on follow mode."
  (interactive)
  (split-into-three-columns)
  (follow-mode 1))

