(ns lopezsolerluis.traducciones
  (:require
   [taoensso.tempura :as tempura :refer [tr]]))

(def translations
  {
   :en {:menu {:archivo "File"
              :crear-perfil-desde-fits "Create profile from FITS file..."
              :crear-perfil-desde-dat "Create profile from DAT file..."
              :abrir-pestaña-annie "Open AnNIE panel..."
              :grabar-pestaña-annie "Save AnNIE panel"
              ; :grabar-pestaña-annie-como "Save AnNIE panel as..."
              :exportar-pdf "PDF export..."
              :cerrar-pestaña "Close panel"
              ; :salir "Exit"
            :edicion "Edit"
              :copiar-perfil "Copy profile"
              :pegar-perfil "Paste profile"
              :cambiar-nombre "Change profile's name..."
              :cambiar-color "Change profile's color..."
              :cambiar-diseño-linea "Change profile's line style..."
              :borrar-perfil "Delete profile..."
              :borrar-etiquetas "Delete labels..."
            :calibracion "Calibrate"
              :auto-calibracion "Auto-calibrate"
            :aritmetica "Arithmetic"
              :para-un-perfil "One profile"
                :normalizacion "Normalization"
                :sumar-uno "Add..."
                :multiplicar-uno "Multiply..."
              :para-dos-perfiles "Two profiles"
                :sumar-dos "Add..."
                :restar-dos "Subtract..."
                :multiplicar-dos "Multiply..."
                :dividir-dos "Divide..."
            :ayuda "Help"
              :creditos "Credits"}
        :ventana-etiqueta {:ok-etiqueta "Ok"
                           :cancel-etiqueta "Cancel"
                           :delete-etiqueta "Delete"
                           :etiqueta-label "Elements:"}
        :ventana-calibración {:ok-calibración "Ok"
                              :cancel-calibración "Cancel"
                              :calibración-título "Calibration"}
        :ventana-espectros {:espectros-título "Reference spectra"
                            :espectros-label "Enter a spectrum class:"
                            :ok-espectros "Ok"
                            :cancel-espectros "Cancel"}
        :confirmar-borrar-etiqueta "Do you really want to delete this label?"
        :etiquetas-no-calibrado "Labels can't be renamed until\nthe profile has been calibrated."
        :debería-seleccionar-dos-líneas "Al least two lines should be selected."
        :deben-ingresarse-dos-lambdas "Both wavelenghts must be input."
        :extensión-no-fits "File does not have .FITS extension"
        :fits-no-simple "FITS file is not SIMPLE"
        :fits-no-valido "FITS file does not seem to be valid"
   }
   :es {:menu {:archivo "Archivo"
              :crear-perfil-desde-fits "Crear perfil desde archivo FITS..."
              :crear-perfil-desde-dat "Crear perfil desde archivo DAT..."
              :abrir-pestaña-annie "Abir pestaña AnNIE"
              :grabar-pestaña-annie "Grabar pestaña annie"
              ; :grabar-pestaña-annie-como "Grabar pestaña annie como..."
              :exportar-pdf "Exportar pestaña AnNIE como PDF..."
              :cerrar-pestaña "Cerrar pestaña"
              ; :salir "Salir"
            :edicion "Edición"
              :copiar-perfil "Copiar perfil"
              :pegar-perfil "Pegar perfil"
              :cambiar-nombre "Cambiar nombre del perfil..."
              :cambiar-color "Cambiar color del perfil..."
              :cambiar-diseño-linea "Cambiar estilo de línea del perfil..."
              :borrar-perfil "Borrar perfil..."
              :borrar-etiquetas "Borrar etiquetas..."
            :calibracion "Calibración"
              :auto-calibracion "Auto-calibración"
            :aritmetica "Aritmética"
              :para-un-perfil "Para 1 perfil"
                :normalizacion "Normalización"
                :sumar-uno "Sumar..."
                :multiplicar-uno "Multiplicar..."
              :para-dos-perfiles "Para 2 perfiles"
                :sumar-dos "Sumar..."
                :restar-dos "Restar..."
                :multiplicar-dos "Multiplicar..."
                :dividir-dos "Dividir..."
            :ayuda "Ayuda"
              :creditos "Créditos"
            }
        :ventana-etiqueta {:ok-etiqueta "Ok"
                           :cancel-etiqueta "Cancelar"
                           :delete-etiqueta "Borrar"
                           :etiqueta-label "Elementos:"}
        :ventana-calibración {:ok-calibración "Ok"
                              :cancel-calibración "Cancelar"
                              :calibración-título "Calibración"}
        :ventana-espectros {:espectros-título "Espectros de referencia"
                            :espectros-label "Ingrese una clase espectral:"
                            :ok-espectros "Ok"
                            :cancel-espectros "Cancelar"}
        :confirmar-borrar-etiqueta "¿Realmente desea borrar esta etiqueta?"
        :etiquetas-no-calibrado "Las etiquetas no pueden renombrarse\nhasta tanto se calibre el perfil."
        :extensión-no-fits "El archivo carece de extensión .FITS"
        :debería-seleccionar-dos-líneas "Debería seleccionar al menos dos líneas."
        :deben-ingresarse-dos-lambdas "Deben ingresarse valores para ambas longitudes de onda."
        :fits-no-simple "El archivo Fits no es SIMPLE"
        :fits-no-valido "El archivo Fits no parece ser válido"
   }
  })

  (defn app-tr
     "Get a localized resource.

     @param resource Resource keyword.
     @param params   Optional positional parameters.

     @return translation of `resource` in active user language or a placeholder."
     [lang resource & params]
       (tr {:dict translations} [lang :en] [resource] (vec params)))
