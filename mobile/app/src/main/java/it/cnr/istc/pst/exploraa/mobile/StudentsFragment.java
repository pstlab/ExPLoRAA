package it.cnr.istc.pst.exploraa.mobile;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.RecyclerView;
import androidx.recyclerview.widget.SortedList;

import it.cnr.istc.pst.exploraa.mobile.ctx.StudentsContext;

public class StudentsFragment extends Fragment {

    public StudentsFragment() {
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        return inflater.inflate(R.layout.fragment_students, container, false);
    }

    private static class StudentView extends RecyclerView.ViewHolder implements View.OnClickListener, StudentsContext.StudentListener {

        private TextView title;
        private StudentsContext.StudentContext student;

        private StudentView(View view) {
            super(view);
            view.setOnClickListener(this);
            title = view.findViewById(R.id.stimulus_title);
        }

        private void setStuent(StudentsContext.StudentContext student) {
            this.student = student;
            title.setText(student.getStudent().getFirstName() + " " + student.getStudent().getLastName());
            student.addStudentListener(this);
        }

        @Override
        public void onClick(View v) {
        }

        @Override
        public void online(boolean on_line) {
        }
    }

    private class StudentsAdapter extends RecyclerView.Adapter<StudentView> {

        private SortedList<StudentsContext.StudentContext> students;

        public StudentsAdapter() {
            this.students = new SortedList<>(StudentsContext.StudentContext.class, new SortedList.Callback<StudentsContext.StudentContext>() {
                @Override
                public int compare(StudentsContext.StudentContext o1, StudentsContext.StudentContext o2) {
                    return (o1.getStudent().getLastName() + " " + o1.getStudent().getFirstName()).compareTo(o2.getStudent().getLastName() + " " + o2.getStudent().getFirstName());
                }

                @Override
                public void onChanged(int position, int count) {
                    notifyItemRangeChanged(position, count);
                }

                @Override
                public boolean areContentsTheSame(StudentsContext.StudentContext oldItem, StudentsContext.StudentContext newItem) {
                    return false;
                }

                @Override
                public boolean areItemsTheSame(StudentsContext.StudentContext item1, StudentsContext.StudentContext item2) {
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
            holder.setStuent(students.get(position));
        }

        @Override
        public int getItemCount() {
            return students.size();
        }
    }
}
