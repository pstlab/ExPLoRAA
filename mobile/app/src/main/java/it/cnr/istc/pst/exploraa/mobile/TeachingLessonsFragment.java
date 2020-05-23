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

import it.cnr.istc.pst.exploraa.api.Message;
import it.cnr.istc.pst.exploraa.mobile.ctx.TeachingLessonsContext;

public class TeachingLessonsFragment extends Fragment {

    public TeachingLessonsFragment() {
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        return inflater.inflate(R.layout.fragment_teaching_lessons, container, false);
    }

    private static class TeachingLessonView extends RecyclerView.ViewHolder implements View.OnClickListener, TeachingLessonsContext.TeachingLessonContext.TeachingLessonListener {

        private TextView title;
        private TeachingLessonsContext.TeachingLessonContext lesson;

        private TeachingLessonView(View view) {
            super(view);
            view.setOnClickListener(this);
            title = view.findViewById(R.id.stimulus_title);
        }

        private void setLesson(TeachingLessonsContext.TeachingLessonContext lesson) {
            this.lesson = lesson;
            title.setText(lesson.getLesson().getName());
            lesson.addTeachingLessonListener(this);
        }

        @Override
        public void onClick(View v) {
        }

        @Override
        public void tokenAdded(Message.Token token) {
        }

        @Override
        public void tokenRemoved(Message.Token token) {
        }

        @Override
        public void tokensCleared() {
        }
    }

    private class TeachingLessonsAdapter extends RecyclerView.Adapter<TeachingLessonView> {

        private SortedList<TeachingLessonsContext.TeachingLessonContext> teaching_lessons;

        public TeachingLessonsAdapter() {
            this.teaching_lessons = new SortedList<>(TeachingLessonsContext.TeachingLessonContext.class, new SortedList.Callback<TeachingLessonsContext.TeachingLessonContext>() {
                @Override
                public int compare(TeachingLessonsContext.TeachingLessonContext o1, TeachingLessonsContext.TeachingLessonContext o2) {
                    return o1.getLesson().getName().compareTo(o2.getLesson().getName());
                }

                @Override
                public void onChanged(int position, int count) {
                    notifyItemRangeChanged(position, count);
                }

                @Override
                public boolean areContentsTheSame(TeachingLessonsContext.TeachingLessonContext oldItem, TeachingLessonsContext.TeachingLessonContext newItem) {
                    return false;
                }

                @Override
                public boolean areItemsTheSame(TeachingLessonsContext.TeachingLessonContext item1, TeachingLessonsContext.TeachingLessonContext item2) {
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
        public TeachingLessonView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new TeachingLessonView(LayoutInflater.from(parent.getContext()).inflate(R.layout.teaching_lesson_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull TeachingLessonView holder, int position) {
            holder.setLesson(teaching_lessons.get(position));
        }

        @Override
        public int getItemCount() {
            return teaching_lessons.size();
        }
    }
}
