FROM rocker/verse:4.0.0

RUN apt-get update && apt-get install -y \
    ranger \
    git \
    tig \
    emacs \
# xdg-utils and ff necessary to display 
# drake-network-graphs from doom-emacs
    xdg-utils \
    firefox \
    && install2.r --error \
       --deps TRUE \
       styler \
    && ln -sf /usr/share/zoneinfo/Europe/Berlin /etc/localtime \
    && git config --global user.email "scheer@freescience.de" \
    && git config --global user.name "Marsel Scheer" \
    && mv /root/.gitconfig /home/rstudio \
    && chown rstudio:rstudio /home/rstudio/.gitconfig \
    && usermod --shell /bin/bash rstudio

USER rstudio

# install and configure doom-emacs
RUN git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d \
    && ~/.emacs.d/bin/doom --yes install \
    && sed -i 's/;;ess/ess/g' ~/.doom.d/init.el \
    && ~/.emacs.d/bin/doom sync \
    && sed -i "s/display-line-numbers-type t/display-line-numbers-type 'relative/g" ~/.doom.d/config.el \
    && mkdir -p /tmp/hostfs \
    && ln -s /tmp/hostfs ~/hostfs


# have no idea why this is the only way
# to add company-dabbrev-code to the 
# backend. Just follow https://github.com/emacs-ess/ESS/issues/955
RUN echo "(require 'company-dabbrev-code)\n\
(add-to-list 'company-backends #'company-dabbrev-code)\n\
(setq company-idle-delay 0)\n\
(setq company-show-numbers t)\n\
(after! ess-r-mode\n\
  (set-company-backend! 'ess-r-mode\n\
    '(:separate company-R-library company-R-args company-R-objects company-dabbrev-code)))" >> ~/.doom.d/config.el

USER root

RUN install2.r --error \
       --deps TRUE \
       drake \
       visNetwork

RUN sed -i "s/(setq doom-theme 'doom-one)/(setq doom-theme 'doom-peacock)/g" ~/.doom.d/config.el
