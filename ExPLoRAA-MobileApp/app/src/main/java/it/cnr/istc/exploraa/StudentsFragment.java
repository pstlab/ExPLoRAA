package it.cnr.istc.exploraa;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.widget.DividerItemDecoration;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

public class StudentsFragment extends Fragment {

    private RecyclerView students_recycler_view;
    private StudentsAdapter students_adapter;

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_students, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        students_recycler_view = view.findViewById(R.id.students_recycler_view);
        students_adapter = new StudentsAdapter(this);

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        students_recycler_view.setHasFixedSize(true);
        students_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        students_recycler_view.setAdapter(students_adapter);
        students_recycler_view.addItemDecoration(new DividerItemDecoration(getContext(), DividerItemDecoration.VERTICAL));
    }

    @Override
    public void onResume() {
        super.onResume();
        ExPLoRAAContext.getInstance().addStudentsListener(students_adapter);
        students_adapter.notifyDataSetChanged();
    }

    @Override
    public void onPause() {
        super.onPause();
        ExPLoRAAContext.getInstance().removeStudentsListener(students_adapter);
    }

    private static class StudentsAdapter extends RecyclerView.Adapter<StudentView> implements ExPLoRAAContext.StudentsListener {

        private StudentsFragment frgmnt;

        private StudentsAdapter(StudentsFragment frgmnt) {
            this.frgmnt = frgmnt;
        }

        @NonNull
        @Override
        public StudentView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new StudentView(frgmnt, LayoutInflater.from(parent.getContext()).inflate(R.layout.student_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull StudentView holder, int position) {
            holder.setStudent(ExPLoRAAContext.getInstance().getStudents().get(position));
        }

        @Override
        public int getItemCount() {
            return ExPLoRAAContext.getInstance().getStudents().size();
        }

        @Override
        public void studentAdded(int pos, StudentContext ctx) {
            notifyItemInserted(pos);
        }

        @Override
        public void studentUpdated(int pos, StudentContext ctx) {
            notifyItemChanged(pos);
        }

        @Override
        public void studentRemoved(int pos, StudentContext ctx) {
            notifyItemRemoved(pos);
        }

        @Override
        public void studentsCleared() {
            notifyDataSetChanged();
        }
    }

    private static class StudentView extends RecyclerView.ViewHolder implements View.OnClickListener {

        private StudentsFragment frgmnt;
        private StudentContext student;
        private TextView name;
        private ImageView student_connection_status_image_view;

        private StudentView(StudentsFragment frgmnt, View view) {
            super(view);
            this.frgmnt = frgmnt;
            view.setOnClickListener(this);
            name = view.findViewById(R.id.student_name);
            student_connection_status_image_view = view.findViewById(R.id.student_connection_status_image_view);
        }

        private void setStudent(StudentContext student) {
            this.student = student;
            name.setText(student.getStudent().first_name + " " + student.getStudent().last_name);
            student_connection_status_image_view.setImageResource(student.isOnLine() ? android.R.drawable.presence_online : android.R.drawable.presence_offline);
        }

        @Override
        public void onClick(View v) {
            // we show the student's details..
            final Intent intent = new Intent(frgmnt.getContext(), StudentActivity.class);
            intent.putExtra("student_id", student.getStudent().id);
            frgmnt.startActivity(intent);
        }
    }
}
