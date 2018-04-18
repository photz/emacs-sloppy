;; -*- lexical-binding: t; debug-on-error: t; -*-

(defconst sloppy-api-base-url "https://api.sloppy.io/v1/apps")
(defconst sloppy-buffer-name "*Sloppy*"
  "Name of the buffer used to display a list of Sloppy.io projects")
(defconst sloppy-details-buffer-name "*Sloppy app details*")

(defvar sloppy-expanded nil)
(defvar sloppy-projects nil)


(require 'json)
(require 'request)
(require 'cl-lib)


(defface sloppy-project
  '((t :weight bold))
  "Face for a Sloppy.io project"
  :group 'sloppy-faces)

(defface sloppy-service
  '()
  "Face for a Sloppy.io service"
  :group 'sloppy-faces)

(defface sloppy-app-offline
  '((t :foreground "dark orange"))
  "Face for a Sloppy.io app that is not running"
  :group 'sloppy-faces)

(defface sloppy-app-online
  '((t :foreground "spring green"))
  "Face for a running Sloppy.io app"
  :group 'sloppy-faces)


(cl-defstruct (sloppy-project (:conc-name sloppy-project.))
  project-id services)

(cl-defstruct (sloppy-service (:conc-name sloppy-service.))
  service-id project-id apps)

(cl-defstruct (sloppy-app (:conc-name sloppy-app.))
  app-id service-id project-id memory image instances volumes version versions domain status env-vars ssl)

(cl-defstruct (sloppy-volume (:conc-name sloppy-volume.))
  name container-path snapshot-count size)


(cl-defun sloppy--parse-volume (volume-raw)
  (let-alist volume-raw
    (make-sloppy-volume
     :name .name
     :container-path .container_path
     :size .size
     :snapshot-count .snapshotCount)))

(cl-defun sloppy--parse-project (project-raw)
  (let-alist project-raw
    (make-sloppy-project
     :project-id .project
     :services (loop for service in (coerce .services 'list)
                     collect (sloppy--parse-service service .project)))))


(cl-defun sloppy--parse-service (service project-id)
  (check-type project-id string)
  (let-alist service
    (make-sloppy-service
     :service-id .id
     :project-id project-id
     :apps (loop for app in (coerce .apps 'list)
                 collect (sloppy--parse-app app project-id .id)))))

(defun sloppy--parse-status (status)
  (check-type status string)
  (cond 
   ((string= "running" status) 'sloppy-app-running)
   ((string= "staged" status) 'sloppy-app-staged)
   ((string= "unhealthy" status) 'sloppy-app-unhealthy)
   ((string= "offline" status) 'sloppy-app-offline
   (otherwise (error (format "Unknown Sloppy.io app status: %s" status))))))


(cl-defun sloppy--parse-app (app project-id service-id)
  (check-type project-id string)
  (check-type service-id string)
  (let-alist app
    (make-sloppy-app
     :app-id .id
     :project-id project-id
     :service-id service-id
     :instances .instances
     :ssl .ssl
     :env-vars .env
     :memory .mem
     :image .image
     :domain .uri.domain
     :version (date-to-time .version)
     :volumes (map 'list 'sloppy--parse-volume .volumes)
     :versions (map 'list 'date-to-time .versions)
     :status (map 'list 'sloppy--parse-status .status))))


(defvar sloppy-mode-map
  (let ((map (make-sparse-keymap 'sloppy-mode-map)))

    (define-key map "r" 'sloppy-restart-app)
    (define-key map "l" 'sloppy-load-projects)
    (define-key map "s" 'sloppy-show-details)
    (define-key map "e" 'sloppy-expand)
    map))


(define-derived-mode sloppy-mode text-mode "sloppy"
  "Major mode for interacting with Sloppy.io"
  (use-local-map sloppy-mode-map))


(defun sloppy ()
  "Interact with your Sloppy.io services."
  (interactive)
  (switch-to-buffer sloppy-buffer-name)
  (sloppy-mode)
  (sloppy-load-projects))


;; Rendering

(defun sloppy--render-projects (projects)
  (with-current-buffer (get-buffer-create sloppy-buffer-name)
    (erase-buffer)
    (dolist (project projects)
      (sloppy--render-project project))
    (pop-to-buffer (current-buffer))))

(defun sloppy--render-service (service)
  (check-type service sloppy-service)
  (let* ((service-id (sloppy-service.service-id service))
         (project-id (sloppy-service.project-id service)))

    (insert (propertize (format " %s\n" service-id)
                        'face 'sloppy-service))

    (dolist (app (sloppy-service.apps service))

      (sloppy--render-app app)

      (when (sloppy--is-expanded app)
        (sloppy--render-app-details app)))))


(defun sloppy--is-expanded (thing)
  (typecase thing
    (sloppy-app
     (let ((app-id (sloppy-app.app-id thing))
           (service-id (sloppy-app.service-id thing))
           (project-id (sloppy-app.project-id thing)))
       (equal sloppy-expanded (list project-id service-id app-id))))))


(defun sloppy--render-project (project)
  (check-type project sloppy-project)
  (insert (propertize (format "%s\n" (sloppy-project.project-id project))
                      'face 'sloppy-project))

  (dolist (service (sloppy-project.services project))
    (sloppy--render-service service)))
    
  
(defun sloppy--render-app-details (app)
  (check-type app sloppy-app)
  (insert (format "   App ID: %s\n" (sloppy-app.app-id app)))
  (insert (format "   Service ID: %s\n" (sloppy-app.service-id app)))
  (insert (format "   Project ID: %s\n" (sloppy-app.project-id app)))
  (insert (format "   SSL: %s\n" (if (sloppy-app.ssl app) "yes" "no")))
  (insert (format "   Image: %s\n" (sloppy-app.image app)))
  (insert (format "   Instances: %d\n" (sloppy-app.instances app)))
  (insert (format "   Domain: %s\n" (sloppy-app.domain app)))
  (insert (format "   Memory: %d\n" (sloppy-app.memory app)))
  (insert "\n   # Environment variables\n")
  (dolist (env-var (coerce (sloppy-app.env-vars app) 'list))
    (cl-destructuring-bind (name . value) env-var
      (insert (format "   %s=%s\n" name value))))
  (insert "\n   # Last failure\n")
  (insert "\n   # Volumes\n")
  (dolist (volume (sloppy-app.volumes app))
    (insert "   Name: %s\n" (sloppy-volume.name volume))
    (insert "   Size: %s\n" (sloppy-volume.size volume))
    (insert "   Container path: %s\n" (sloppy-volume.container-path volume))
    (insert "   Snapshot count: %d\n" (sloppy-volume.snapshot-count volume))))


(defun sloppy--render-app (app)
  (check-type app sloppy-app)
  (let* ((version-since (time-since (sloppy-app.version app)))
         (version-seconds-since (time-to-seconds version-since))
         (up (format-seconds "%dd %hh %mm %ss" version-seconds-since))
         (status (car (sloppy-app.status app)))
         (status-icon (case status
                        (sloppy-app-running "⏵")
                        (sloppy-app-staged "⏸")
                        (sloppy-app-unhealthy "☠")
                        (sloppy-app-offline "⏹")
                        (t "⏹")))
         (is-online (eq status 'sloppy-app-running))
         (the-face (if is-online 'sloppy-app-online 'sloppy-app-offline))
         (text (format "  %s %s (%s)\n"
                       status-icon (sloppy-app.app-id app) up)))
                  
    (insert (propertize text
                        'project-id (sloppy-app.project-id app)
                        'service-id (sloppy-app.service-id app)
                        'app-id (sloppy-app.app-id app)
                        'face the-face))))

;;
;; Interactive functions
;;

(defun sloppy-restart-app ()
  (interactive)

  (unless (or (eq (get-text-property (point) 'face) 'sloppy-app-online)
              (eq (get-text-property (point) 'face) 'sloppy-app-offline))
    (message "No Sloppy.io app selected")
    (return-from 'sloppy-restart-app))

  (let* ((project-id (get-text-property (point) 'project-id))
         (service-id (get-text-property (point) 'service-id))
         (app-id (get-text-property (point) 'app-id)))
    
    (message "About to restart app...")
    (sloppy--restart-app
     project-id service-id app-id
     :success
     '(lambda (s p a)
        (message "App successfully restarted")
        (sloppy--get-projects
         :success
         '(lambda (projects)
            (setq sloppy-projects projects)
            (sloppy--render-projects sloppy-projects))))
     :error
     '(lambda (s p a)
        (message (format "Unable to restart app %s" a))))))
  

          

(defun sloppy-load-projects ()
  (interactive)
  (message "Requesting projects...")
  (with-current-buffer (get-buffer-create sloppy-buffer-name)
    (erase-buffer)
    (insert "Loading...\n"))
  (sloppy--get-projects
   :success
   '(lambda (projects)
      (setq sloppy-projects projects)
      (sloppy--render-projects sloppy-projects))))





(defun sloppy-expand ()
  (interactive)

  (unless (or (eq (get-text-property (point) 'face) 'sloppy-app-online)
              (eq (get-text-property (point) 'face) 'sloppy-app-offline))
    (message "No Sloppy.io app selected")
    (return-from 'sloppy-expand))

  (let* ((project-id (get-text-property (point) 'project-id))
         (service-id (get-text-property (point) 'service-id))
         (app-id (get-text-property (point) 'app-id))
         (token (list project-id service-id app-id))
         (current-line (line-number-at-pos)))
            
    ;; collapse the currently expanded app or expand the selected one
    (setq sloppy-expanded (if (equal sloppy-expanded token) nil token))

    ;; rerender proejcts
    (sloppy--render-projects sloppy-projects)

    ;; restore the point position of the point
    (goto-line current-line)))
      

(defun sloppy-show-details ()
  (interactive)
  
  (unless (or (eq (get-text-property (point) 'face) 'sloppy-app-online)
              (eq (get-text-property (point) 'face) 'sloppy-app-offline))
    (message "No Sloppy.io app selected")
    (return-from 'sloppy-show-details))

  (let* ((project-id (get-text-property (point) 'project-id))
         (service-id (get-text-property (point) 'service-id))
         (app-id (get-text-property (point) 'app-id)))

    (sloppy--get-app
     project-id service-id app-id
     :success
     '(lambda (app)
        (with-current-buffer
            (get-buffer-create sloppy-details-buffer-name)
          
          (erase-buffer)
          (sloppy--render-app-details app)
          (display-buffer sloppy-details-buffer-name))))))
  
;;
;; API
;;


(cl-defun sloppy--get-projects (&key success)
  (check-type success function)
  (request
   sloppy-api-base-url
   :parser 'json-read
   :headers `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer " sloppy-api-token)))
   :success
   (cl-function
    (lambda (&key data &allow-other-keys)
      (funcall success (map 'list 'sloppy--parse-project (alist-get 'data data)))))
   :error
   (cl-function
    (lambda (&rest args &key error-thrown &allow-other-keys)
      (message "Got error: %S" error-thrown)))))


(cl-defun sloppy--restart-app (project-id service-id app-id &key success error)
  (check-type project-id string)
  (check-type service-id string)
  (check-type app-id string)
  (let ((url (format "%s/%s/services/%s/apps/%s/restart"
                     sloppy-api-base-url project-id service-id app-id)))
    (request
     url
     :type "POST"
     :parser 'json-read
     :headers `(("Content-Type" . "application/json")
                ("Authorization" . ,(concat "Bearer " sloppy-api-token)))
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (funcall success project-id service-id app-id)))
     :error
     (cl-function
      (lambda (&rest args &key error-thrown &allow-other-keys)
        (when (functionp error)
          (funcall error project-id service-id app-id)))))))


(cl-defun sloppy--get-app (project-id service-id app-id &key success)
  (check-type project-id string)
  (check-type service-id string)
  (check-type app-id string)
  (let ((url (format "%s/%s/services/%s/apps/%s"
                     sloppy-api-base-url project-id service-id app-id)))
    (request
     url
     :parser 'json-read
     :headers `(("Content-Type" . "application/json")
                ("Authorization" . ,(concat "Bearer " sloppy-api-token)))
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (when success
          (funcall success (sloppy--parse-app (alist-get 'data data) project-id service-id))))))))



(provide 'sloppy)

