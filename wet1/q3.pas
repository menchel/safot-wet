program FacultyManagment;

type
    typeOfCourse = (REG, LAB, SEM);

//variant record of a course
type 
    course = record
        id: integer;
        name: string;
        case courseType: typeOfCourse of
            REG: (lectureHours: integer);
            LAB: (manager: string; place: string);
            SEM: (startHour: string; endHour: string);
    end;

    var courses: array [0..100] of course;
    var action: string;

    //printing course's details
    procedure printCourse(var courses: array of course);
    var id:integer;
    begin
        ReadLn(id);
        if courses[id-1000].courseType=REG then
            writeln('REG');
        if courses[id-1000].courseType=LAB then
            writeln('LAB');
        if courses[id-1000].courseType=SEM then
            writeln('SEM');
        writeln(courses[id-1000].id);
        writeln(courses[id-1000].name);
        if courses[id-1000].courseType=REG then
            write(courses[id-1000].lectureHours);
        if courses[id-1000].courseType=LAB then
        begin
            writeln(courses[id-1000].manager);
            write(courses[id-1000].place);
        end;
        if courses[id-1000].courseType=SEM then
        begin;
            writeln(courses[id-1000].startHour);
            write(courses[id-1000].endHour);
        end;
        writeln()
    end;

    //adds a course from seminar type
    procedure addSeminarCourse(var courses: array of course;id:integer;var name:string);
    var startHours:string;
    var endHours:string;
    var courseItem:course;
    begin
        ReadLn(startHours);
        ReadLn(endHours);
        courseItem.id:=id;
        courseItem.name:=name;
        courseItem.courseType:=SEM;
        courseItem.startHour:=startHours;
        courseItem.endHour:=endHours;
        courses[id-1000]:=courseItem;
    end;

    //adds a course from lab type
    procedure addLabCourse(var courses: array of course;id:integer;var name:string);
    var manager:string;
    var place:string;
    var courseItem:course;
    begin
        ReadLn(manager);
        ReadLn(place);
        courseItem.id:=id;
        courseItem.name:=name;
        courseItem.courseType:=LAB;
        courseItem.manager:=manager;
        courseItem.place:=place;
        courses[id-1000]:=courseItem;
    end;

    //adds a course from regular type
    procedure addRegularCourse(var courses: array of course;id:integer;var name:string);
    var lectureHours:integer;
    var courseItem:course;
    begin
        ReadLn(lectureHours);
        courseItem.id:=id;
        courseItem.name:=name;
        courseItem.courseType:=REG;
        courseItem.lectureHours:=lectureHours;
        courses[id-1000]:=courseItem;
    end;

    //adds a general course
    procedure addCourse(var courses: array of course);
    var courseType:typeOfCourse;
    var id:integer;
    var name:string;
    var courseTypeString:string;
    begin
        ReadLn(courseTypeString);
        ReadLn(id);
        ReadLn(name);

        if courseTypeString = 'REG' then
            courseType := REG;
        if courseTypeString = 'LAB' then
            courseType := LAB;
        if courseTypeString = 'SEM' then
            courseType := SEM;;

        if courseType=REG then
            addRegularCourse(courses,id,name);
        if courseType=LAB then
            addLabCourse(courses,id,name);
        if courseType=SEM then
            addSeminarCourse(courses,id,name);
    end;

begin
    ReadLn(action);
    while not (action='END') do
    begin
        if action='ADD' then
            addCourse(courses);
        if action='PRINT' then
            printCourse(courses);
        ReadLn(action);
    end;
end.