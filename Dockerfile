FROM rapporteket/base-r:main

WORKDIR /app/R

COPY *.tar.gz .

RUN R -e "remotes::install_local(list.files(pattern = \"*.tar.gz\"))" \
  && rm ./*.tar.gz

EXPOSE 3838

CMD ["R", "-e", "options(shiny.port = 3838, shiny.host = \"0.0.0.0\"); nnrr::nnrrApp()"]

