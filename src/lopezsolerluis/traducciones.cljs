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
              :exportar-como "Export tab as..."
                :jpeg "JPEG"
                :png "PNG"
                :svg "SVG"
              :cerrar-pestaña "Close tab"
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
        :ventana-zoom-etc {:perfil-activo "Active profile"
                           :desplazar "Move"}
        :help-window {:help-baricentro "To obtain the barycenter of a line: "
                      :help-texto-etiquetas "To change the text of the labels: "
                      :help-mover-etiquetas "To move the labels: "
                      :help-consejo "Tip:"
                      :help-controles "Controls:"
                      :help-consejo-grabar "To save a tab with a name of your choice, configure your browser to ask for the place to download files. 😉"}
        :la-clase-es-desconocida "This is not an available reference spectral class"
        :confirmar-borrar-etiqueta "Do you really want to delete this label?"
        :etiquetas-no-calibrado "Labels can't be renamed until\nthe profile has been calibrated."
        :debería-seleccionar-dos-líneas "Al least two lines should be selected."
        :no-hay-perfil-para-calibrar "There is no profile to calibrate."
        :deben-ingresarse-dos-lambdas "Both wavelenghts must be input."
        :extensión-no-fits "File does not have .FITS extension"
        :fits-no-simple "FITS file is not SIMPLE"
        :fits-no-valido "FITS file does not seem to be valid"
        :annie-no-válido "ANNIE file does not seem to be valid"
        :no-pestaña-activa-para-grabar "There is no tab to save"
        :perfil-no-calibrado-no-puede-copiarse "An uncalibrated profile can't be copied to the clipboard."
        :no-hay-perfil-que-copiar "There is no profile to copy."
        :perfil-no-calibrado-no-admite-pegado "An uncalibrated profile can't accept a pasted profile."
        :portapapeles-vacío "The clipboard is empty."
        :perfil-copiado "The profile has been copied to the clipboard."
   }
   :es {:menu {:archivo "Archivo"
              :crear-perfil-desde-fits "Crear perfil desde archivo FITS..."
              :crear-perfil-desde-dat "Crear perfil desde archivo DAT..."
              :abrir-pestaña-annie "Abir pestaña AnNIE"
              :grabar-pestaña-annie "Grabar pestaña annie"
              :exportar-como "Exportar pestaña AnNIE como..."
              :cerrar-pestaña "Cerrar pestaña"
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
        :ventana-zoom-etc {:perfil-activo "Perfil activo"
                           :desplazar "Desplazar"}
        :help-window {:help-baricentro "Para determinar el baricentro de una línea: "
                      :help-texto-etiquetas "Para cambiar el texto de las etiquetas: "
                      :help-mover-etiquetas "Para mover las etiquetas: "                      
                      :help-consejo "Consejo:"
                      :help-controles "Controles:"
                      :help-consejo-grabar "Si querés grabar las pestañas con el nombre que elijas, configurá tu navegador para que te pregunte siempre dónde debe descargar los archivos. 😉"}
        :la-clase-es-desconocida "No se encuentra entre las clases espectrales de referencia."
        :confirmar-borrar-etiqueta "¿Realmente desea borrar esta etiqueta?"
        :etiquetas-no-calibrado "Las etiquetas no pueden renombrarse\nhasta tanto se calibre el perfil."
        :extensión-no-fits "El archivo carece de extensión .FITS"
        :debería-seleccionar-dos-líneas "Debería seleccionar al menos dos líneas."
        :no-hay-perfil-para-calibrar "No hay perfil que calibrar."
        :deben-ingresarse-dos-lambdas "Deben ingresarse valores para ambas longitudes de onda."
        :fits-no-simple "El archivo FITS no es SIMPLE"
        :fits-no-valido "El archivo FITS no parece ser válido"
        :annie-no-válido "El archivo ANNIE no parece ser válido"
        :no-pestaña-activa-para-grabar "No hay pestaña que grabar"
        :perfil-no-calibrado-no-puede-copiarse "Un perfil no calibrado no puede copiarse al portapapeles."
        :no-hay-perfil-que-copiar "No hay perfil que copiar."
        :perfil-no-calibrado-no-admite-pegado "No puede pegarse un perfil en una pestaña no calibrada."
        :portapapeles-vacío "El portapapeles está vacío."
        :perfil-copiado "El perfil ha sido copiado al portapapeles."
   }
  })

  (defn app-tr
     "Get a localized resource.

     @param resource Resource keyword.
     @param params   Optional positional parameters.

     @return translation of `resource` in active user language or a placeholder."
     [lang resource & params]
       (tr {:dict translations} [lang :en] [resource] (vec params)))
