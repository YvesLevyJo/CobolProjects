CREATE DATABASE school;
USE school;

CREATE TABLE STUDENT(
    id SERIAL,
    lastname CHAR(30) NOT NULL DEFAULT 'DUPOND',
    firstname CHAR(30) NOT NULL DEFAULT 'MonsieurMadame',
    age     SMALLINT,
    CONSTRAINT id_student PRIMARY KEY (id)
);

CREATE TABLE  COURSE(
    id      SERIAL,
    label   CHAR(21),
    coef    NUMERIC(3,1) NOT NULL DEFAULT 1,
    CONSTRAINT  id_course PRIMARY KEY (id)
);

CREATE TABLE GRADE(
    id SERIAL,
    grade_label CHAR(25),
    grade_student_id SMALLINT,
    grade_course_id SMALLINT,
    FOREIGN KEY (grade_student_id) REFERENCES STUDENT(id),
    FOREIGN KEY (grade_course_id) REFERENCES COURSE(id)
);