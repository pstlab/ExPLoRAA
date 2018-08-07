package it.cnr.istc.exploraa;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
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

import java.util.ArrayList;
import java.util.List;

public class StudentsFragment extends Fragment {

    private RecyclerView students_recycler_view;
    private final StudentsAdapter students_adapter = new StudentsAdapter();
    private BroadcastReceiver student_added_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            students_adapter.notifyItemInserted(intent.getIntExtra("position", 0));
        }
    };
    private BroadcastReceiver student_updated_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            assert getActivity() != null;
            final StudentContext student = ((MainActivity) getActivity()).service.getStudent(intent.getLongExtra("student", 0));
            students_adapter.notifyItemChanged(students_adapter.students.indexOf(student));
        }
    };
    private BroadcastReceiver student_removed_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            students_adapter.notifyItemRemoved(intent.getIntExtra("position", 0));
        }
    };
    private BroadcastReceiver students_cleared_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            students_adapter.notifyDataSetChanged();
        }
    };

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        assert getActivity() != null;
        getActivity().registerReceiver(student_added_receiver, new IntentFilter(ExPLoRAAService.ADDED_STUDENT));
        getActivity().registerReceiver(student_updated_receiver, new IntentFilter(StudentContext.UPDATED_STUDENT));
        getActivity().registerReceiver(student_removed_receiver, new IntentFilter(ExPLoRAAService.REMOVED_STUDENT));
        getActivity().registerReceiver(students_cleared_receiver, new IntentFilter(ExPLoRAAService.CLEARED_FOLLOWING_LESSONS));
    }

    @Override
    public void onDestroy() {
        super.onDestroy();

        assert getActivity() != null;
        getActivity().unregisterReceiver(student_added_receiver);
        getActivity().unregisterReceiver(student_updated_receiver);
        getActivity().unregisterReceiver(student_removed_receiver);
        getActivity().unregisterReceiver(students_cleared_receiver);
    }

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_students, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        students_recycler_view = view.findViewById(R.id.students_recycler_view);

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        students_recycler_view.setHasFixedSize(true);
        students_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        students_recycler_view.setAdapter(students_adapter);
        students_recycler_view.addItemDecoration(new DividerItemDecoration(getContext(), DividerItemDecoration.VERTICAL));
    }

    void setStudents(List<StudentContext> students) {
        students_adapter.setStudents(students);
    }

    private class StudentsAdapter extends RecyclerView.Adapter<StudentView> {

        private List<StudentContext> students = new ArrayList<>();

        private void setStudents(List<StudentContext> students) {
            this.students = students;
            notifyDataSetChanged();
        }

        @NonNull
        @Override
        public StudentView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new StudentView(LayoutInflater.from(parent.getContext()).inflate(R.layout.student_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull StudentView holder, int position) {
            holder.setStudent(students.get(position));
        }

        @Override
        public int getItemCount() {
            return students.size();
        }
    }

    private class StudentView extends RecyclerView.ViewHolder implements View.OnClickListener {

        private StudentContext student;
        private TextView name;
        private ImageView student_connection_status_image_view;

        private StudentView(View view) {
            super(view);
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
            final Intent intent = new Intent(getContext(), StudentActivity.class);
            intent.putExtra("student_id", student.getStudent().id);
            startActivity(intent);
        }
    }
}
