package it.cnr.istc.exploraa;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.util.SortedList;
import android.support.v7.widget.DividerItemDecoration;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import java.util.List;

public class StudentsFragment extends Fragment implements ExPLoRAAService.StudentsListener {

    private RecyclerView students_recycler_view;
    private final StudentsAdapter students_adapter = new StudentsAdapter();

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        ExPLoRAAContext.getInstance().getService().addStudentsListener(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (ExPLoRAAContext.getInstance().isServiceRunning())
            ExPLoRAAContext.getInstance().getService().removeStudentsListener(this);
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
        students_adapter.students.addAll(students);
    }

    @Override
    public void studentAdded(int pos, StudentContext ctx) {
        students_adapter.students.add(ctx);
    }

    @Override
    public void studentUpdated(int pos, StudentContext ctx) {
        students_adapter.students.updateItemAt(indexOf(ctx), ctx);
    }

    @Override
    public void studentRemoved(int pos, StudentContext ctx) {
        students_adapter.students.remove(ctx);
    }

    @Override
    public void studentsCleared() {
        students_adapter.students.clear();
    }

    private int indexOf(StudentContext ctx) {
        for (int i = 0; i < students_adapter.students.size(); i++)
            if (ctx.equals(students_adapter.students.get(i)))
                return i;
        return -1;
    }

    private class StudentsAdapter extends RecyclerView.Adapter<StudentView> {

        private SortedList<StudentContext> students;

        public StudentsAdapter() {
            this.students = new SortedList<>(StudentContext.class, new SortedList.Callback<StudentContext>() {
                @Override
                public int compare(StudentContext o1, StudentContext o2) {
                    return (o1.getStudent().last_name + " " + o1.getStudent().first_name).compareTo(o2.getStudent().last_name + " " + o2.getStudent().first_name);
                }

                @Override
                public void onChanged(int position, int count) {
                    notifyItemRangeChanged(position, count);
                }

                @Override
                public boolean areContentsTheSame(StudentContext oldItem, StudentContext newItem) {
                    return false;
                }

                @Override
                public boolean areItemsTheSame(StudentContext item1, StudentContext item2) {
                    return item1 == item2;
                }

                @Override
                public void onInserted(int position, int count) {
                    notifyItemRangeInserted(position, count);
                }

                @Override
                public void onRemoved(int position, int count) {
                    notifyItemRangeRemoved(position, count);
                }

                @Override
                public void onMoved(int fromPosition, int toPosition) {
                    notifyItemMoved(fromPosition, toPosition);
                }
            });
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
            name.setText(getString(R.string.full_name, student.getStudent().first_name, student.getStudent().last_name));
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
