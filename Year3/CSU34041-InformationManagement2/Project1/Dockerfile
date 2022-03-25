FROM mysql
ENV MYSQL_ROOT_PASSWORD "dev"
COPY transnat_rail.sql /docker-entrypoint-initdb.d
RUN chown -R mysql:mysql /docker-entrypoint-initdb.d/
EXPOSE 3306
