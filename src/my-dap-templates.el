;;; Debug templates for each language

;;; Python
(defun my-register-python-dap-templates ()
  "Register my custom python dap templates"
  (dap-register-debug-template
   "Raccordement"
   (list :type "python"
	 :args "-i"
	 :cwd nil
	 :env '(("DEBUG" . "1"))
	 :target-module (expand-file-name "~/Documents/raccordement/example_nantes.py")
	 :request "launch"
	 :name "Raccordement"))
  
  (dap-register-debug-template
   "Python :: Run sirao_cli"
   (list :type "python"
         :args "network connection-options Nantes_Metropole.sqlite sites_nantes_id.csv \"Situation été\" Original REDACTED full_output"
         :cwd nil
         :module "sirao_cli"
         :program nil
         :request "launch"
         :name "Python :: Run sirao_cli"))
  
  (dap-register-debug-template
   "Core :: Run pytest (at point)"
   (list :type "python-test-at-point"
         :args ""
	 :env '(("GMAPS_API_KEY" . "REDACTED"))
         :program nil
         :module "pytest"
         :request "launch"
         :name "Core :: Run pytest (at point)"))
  (dap-register-debug-template
   "Python :: Run file (buffer)"
   (list :type "python"
         :args ""
         :cwd nil
         :module nil
         :program nil
	 :justMyCode nil
         :request "launch"
         :name "Python :: Run file (buffer)")))

(provide 'my-dap-templates)
