(ns lopezsolerluis.traducciones
  (:require
   [taoensso.tempura :as tempura :refer [tr]]))

(def translations
  {
   :en {:menu {:archivo "File"
              :crear-perfil-desde-fits "Create profile from FITS file..."
              :crear-perfil-desde-dat "Create profile from DAT file..."
              :abrir-pestaña-annie "Open AnNIE tab..."
              :grabar-pestaña-annie "Save AnNIE tab"
              :exportar-como "Export tab as..."
                :jpeg "JPEG"
                :png "PNG"
                :svg "SVG"
            :edicion "Edit"
              :copiar-perfil "Copy profile"
              :pegar-perfil "Paste profile"
              :cambiar-perfil "Modify profile..."
              ; :cambiar-nombre "Change profile's name..."
              ; :cambiar-color "Change profile's color..."
              ; :cambiar-diseño-linea "Change profile's line style..."
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
              :controles "Controls and tips"
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
        :ventana-zoom-etc {:perfil-activo "Active profile:"
                           :desplazar "Move"
                           :escalar "Scale"}
        :ventana-cambiar-perfil {:perfil-activo-cambiar "Active profile:"
                                 :cambiar-nombre-perfil-label "Name:"
                                 :boton-cambiar-nombre-perfil "Change"
                                 :cambiar-color-perfil-label "Color:"
                                 :cambiar-ancho-perfil-label "Width:"
                                 :cambiar-estilo-perfil-label "Style:"
                                 :o-por-defecto "or default value"}
        :ventana-operar-uno {:conservar-etiquetas-uno-label "Keep labels?"
                             :cancel-operar-uno "Cancel"}
        :ventana-operar-dos {:perfil-activo-operar-dos-título "Operate with two profiles"
                             :conservar-etiquetas-dos-label "Keep labels?"
                             :cancel-operar-dos "Cancel"}
        :debe-haber-al-menos-dos-perfiles "There must be al least two profiles in the tab to operate."
        :sumar-uno-título "Add to the active profile:"
        :multiplicar-uno-título "Multiply to the active profile:"
        :ventana-borrar-perfil {:perfiles-lista-label "Choose a profile to delete:"
                                :cancel-perfiles-borrar "Cancel"}
        :help-window {:help-baricentro "To obtain the barycenter of a line: "
                      :help-texto-etiquetas "To change the text of the labels or delete them: "
                      :help-mover-etiquetas "To move the labels: "
                      :help-consejo "Tips:"
                      :help-controles "Controls:"
                      :help-consejo-grabar "To save a tab with a name of your choice, configure your browser to ask for the place to download files. 😉"
                      :help-consejo-exportar "Up to now, to get an image with the results of your work, I have to suggest you take a screenshot...😳"}
        :credits-window {:credits-version "Version"
                         :credits-agradecimientos "Acknowledgments:"
                         :credits-uno "To A.J. Pickles et al. (1998), for their magnificents reference spectra."
                         :credits-dos "To Diego Guberman, for his generous and precises suggestions."
                         :descargar-annie "The manual (in spanish) of the original version of this program can be obtained"
                         :enlace-annie "here"
                         :credits-nota-uno "[1] In order to get this application into the smallest possible size, the spectra were limited to the classes OBAFGKM, and in the 3800Å - 7500Å range."}
        :la-clase-es-desconocida "This is not an available reference spectral class"
        :confirmar-borrar-etiqueta "Do you really want to delete this label?"
        :etiquetas-no-calibrado "Labels can't be renamed until\nthe profile has been calibrated."
        :debería-seleccionar-dos-líneas "Al least two lines should be selected."
        :no-hay-perfil-para-calibrar "There is no profile to calibrate."
        :deben-ingresarse-dos-lambdas "Both wavelenghts must be input."
        :extensión-no-fits "File does not have .FITS extension"
        :fits-no-simple "FITS file is not SIMPLE"
        :fits-no-valido "FITS file does not seem to be valid"
        :annie-no-válido "The file does not seem to be valid"
        :no-pestaña-activa-para-grabar "There is no tab to save"
        :perfil-no-calibrado-no-puede-copiarse "An uncalibrated profile can't be copied to the clipboard."
        :no-hay-perfil-que-copiar "There is no profile to copy."
        :perfil-no-calibrado-no-admite-pegado "An uncalibrated profile can't accept a pasted profile."
        :portapapeles-vacío "The clipboard is empty."
        :perfil-copiado "The profile has been copied to the clipboard."
        :confirmar-borrar-pestaña "Do you really want to delete this tab? This can't be undone..."
        :confirmar-borrar-perfil  "Do you really want to delete this profile? This can't be undone..."
        :confirmar-borrar-etiquetas "Do you really want to delete all the labels of this profile? This can't be undone..."
        :no-hay-perfil-que-modificar "There is no profile to modify."
        :no-hay-etiquetas-que-borrar "There are no labels to delete."
        :el-nombre-pertenece-a-un-perfil-de-la-pestaña "The name chosen already belongs to one of this tab's profiles."
        :normalizado ".normalized"
        :debe-ingresarse-un-número "The input must be a number."
        :suma-escalar "-.calar.add"
        :producto-escalar ".escalar.product"
        :suma-dos-perfiles ".add."
        :diferencia-dos-perfiles ".minus."
        :producto-dos-perfiles ".multiplied."
        :cociente-dos-perfiles ".divided."
        :no-hay-perfiles-que-borrar "There are no profiles to delete."
        :el-perfil-activo-no-puede-borrarse "The active profile cannot be deleted. If you wish, you can delete the entire tab."
        :dispersión "Dispersion:"
        :xpixel "/ pixel"
   }
   :es {:menu {:archivo "Archivo"
              :crear-perfil-desde-fits "Crear perfil desde archivo FITS..."
              :crear-perfil-desde-dat "Crear perfil desde archivo DAT..."
              :abrir-pestaña-annie "Abrir pestaña AnNIE"
              :grabar-pestaña-annie "Grabar pestaña AnNIE"
              :exportar-como "Exportar pestaña AnNIE como..."
            :edicion "Edición"
              :copiar-perfil "Copiar perfil"
              :pegar-perfil "Pegar perfil"
              :cambiar-perfil "Modificar perfil..."
              ; :cambiar-nombre "Cambiar nombre del perfil..."
              ; :cambiar-color "Cambiar color del perfil..."
              ; :cambiar-diseño-linea "Cambiar estilo de línea del perfil..."
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
              :controles "Controles y consejos"
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
        :ventana-zoom-etc {:perfil-activo "Perfil activo:"
                           :desplazar "Desplazar"
                           :escalar "Escalar"}
        :ventana-cambiar-perfil {:perfil-activo-cambiar "Perfil activo:"
                                 :cambiar-nombre-perfil-label "Nombre:"
                                 :boton-cambiar-nombre-perfil "Cambiar"
                                 :cambiar-color-perfil-label "Color:"
                                 :cambiar-ancho-perfil-label "Ancho:"
                                 :cambiar-estilo-perfil-label "Estilo:"
                                 :o-por-defecto "o por defecto"}
        :ventana-operar-uno {:conservar-etiquetas-uno-label "¿Heredar etiquetas"
                             :cancel-operar-uno "Cancelar"}
        :ventana-operar-dos {:perfil-activo-operar-dos-título "Operar con dos perfiles"
                             :conservar-etiquetas-dos-label "¿Heredar etiquetas"
                             :cancel-operar-dos "Cancelar"}
        :debe-haber-al-menos-dos-perfiles "Debe haber al menos dos perfiles en la pestaña para operar."
        :sumar-uno-título "Sumar al perfil activo:"
        :multiplicar-uno-título "Multiplicar el perfil activo:"
        :ventana-borrar-perfil {:perfiles-lista-label "Elija un perfil para borrar:"
                                :cancel-perfiles-borrar "Cancelar"}
        :help-window {:help-baricentro "Para determinar el baricentro de una línea: "
                      :help-texto-etiquetas "Para cambiar el texto de las etiquetas o borrarlas: "
                      :help-mover-etiquetas "Para mover las etiquetas: "
                      :help-consejo "Consejos:"
                      :help-controles "Controles:"
                      :help-consejo-grabar "Si querés grabar las pestañas con el nombre que elijas, configurá tu navegador para que te pregunte siempre dónde debe descargar los archivos. 😉"
                      :help-consejo-exportar "Por ahora, para obtener una imagen del fruto de tu trabajo, debo sugerirte que hagas una captura de pantalla...😳"}
        :credits-window {:credits-version "Versión"
                         :credits-agradecimientos "Agradecimientos:"
                         :credits-uno "a A.J. Pickles et al. (1998), por sus magníficos espectros de referencia."
                         :credits-dos "a Diego Guberman, por sus generosas y precisas sugerencias."
                         :descargar-annie "El manual de la versión original de este programa puede descargarse"
                         :enlace-annie "aquí"
                         :credits-nota-uno "[1] A fin de mantener la presente aplicación dentro del menor tamaño posible, los espectros originales se limitaron a las clases OBAFGKM, y sólo presentan datos en el intervalo 3800Å - 7500Å."}
        :la-clase-es-desconocida "No se encuentra entre las clases espectrales de referencia."
        :confirmar-borrar-etiqueta "¿Realmente desea borrar esta etiqueta?"
        :etiquetas-no-calibrado "Las etiquetas no pueden renombrarse\nhasta tanto se calibre el perfil."
        :extensión-no-fits "El archivo carece de extensión .FITS"
        :debería-seleccionar-dos-líneas "Debería seleccionar al menos dos líneas."
        :no-hay-perfil-para-calibrar "No hay perfil que calibrar."
        :deben-ingresarse-dos-lambdas "Deben ingresarse valores para ambas longitudes de onda."
        :fits-no-simple "El archivo FITS no es SIMPLE"
        :fits-no-valido "El archivo FITS no parece ser válido"
        :annie-no-válido "El archivo no parece ser válido"
        :no-pestaña-activa-para-grabar "No hay pestaña que grabar"
        :perfil-no-calibrado-no-puede-copiarse "Un perfil no calibrado no puede copiarse al portapapeles."
        :no-hay-perfil-que-copiar "No hay perfil que copiar."
        :no-hay-etiquetas-que-borrar "No hay etiquetas que borrar."
        :perfil-no-calibrado-no-admite-pegado "No puede pegarse un perfil en una pestaña no calibrada."
        :portapapeles-vacío "El portapapeles está vacío."
        :perfil-copiado "El perfil ha sido copiado al portapapeles."
        :confirmar-borrar-pestaña "¿Realmente desea borrar esta pestaña? Esta acción no puede deshacerse..."
        :confirmar-borrar-perfil  "¿Realmente desea borrar este perfil? Esta acción no puede deshacerse..."
        :confirmar-borrar-etiquetas "¿Realmente desea borrar todas las etiquetas de este perfil? Esta acción no puede deshacerse..."
        :no-hay-perfil-que-modificar "No hay perfil que modificar."
        :el-nombre-pertenece-a-un-perfil-de-la-pestaña "El nombre ingresado ya pertenece a un perfil de esta pestaña."
        :normalizado ".normalizado"
        :debe-ingresarse-un-número "Debe ingresarse un número."
        :suma-escalar ".suma.escalar"
        :producto-escalar ".producto.escalar"
        :suma-dos-perfiles ".sumado."
        :diferencia-dos-perfiles ".menos."
        :producto-dos-perfiles ".multiplicado."
        :cociente-dos-perfiles ".dividido."
        :no-hay-perfiles-que-borrar "No hay perfiles que borrar."
        :el-perfil-activo-no-puede-borrarse "El perfil activo no puede borrarse. Si así lo desea, puede borrar la pestaña completa."
        :dispersión "Dispersión:"
        :xpixel "/ pixel"
   }
  })

  (defn app-tr
     "Get a localized resource.

     @param resource Resource keyword.
     @param params   Optional positional parameters.

     @return translation of `resource` in active user language or a placeholder."
     [lang resource & params]
       (tr {:dict translations} [lang :en] [resource] (vec params)))
