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
import it.cnr.istc.pst.exploraa.mobile.ctx.FollowingLessonsContext;

public class FollowingLessonsFragment extends Fragment {

    public FollowingLessonsFragment() {
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        return inflater.inflate(R.layout.fragment_following_lessons, container, false);
    }

    private static class FollowingLessonView extends RecyclerView.ViewHolder implements View.OnClickListener, FollowingLessonsContext.FollowingLessonContext.FollowingLessonListener {

        private TextView title;
        private FollowingLessonsContext.FollowingLessonContext lesson;

        private FollowingLessonView(View view) {
            super(view);
            view.setOnClickListener(this);
            title = view.findViewById(R.id.stimulus_title);
        }

        private void setLesson(FollowingLessonsContext.FollowingLessonContext lesson) {
            this.lesson = lesson;
            title.setText(lesson.getLesson().getName());
            lesson.addFollowingLessonListener(this);
        }

        @Override
        public void onClick(View v) {
        }

        @Override
        public void stimulusAdded(Message.Stimulus stimulus) {
        }

        @Override
        public void stimulusRemoved(Message.Stimulus stimulus) {
        }

        @Override
        public void stimuliCleared() {
        }
    }

    private class FollowingLessonsAdapter extends RecyclerView.Adapter<FollowingLessonView> {

        private SortedList<FollowingLessonsContext.FollowingLessonContext> following_lessons;

        public FollowingLessonsAdapter() {
            this.following_lessons = new SortedList<>(FollowingLessonsContext.FollowingLessonContext.class, new SortedList.Callback<FollowingLessonsContext.FollowingLessonContext>() {
                @Override
                public int compare(FollowingLessonsContext.FollowingLessonContext o1, FollowingLessonsContext.FollowingLessonContext o2) {
                    return o1.getLesson().getName().compareTo(o2.getLesson().getName());
                }

                @Override
                public void onChanged(int position, int count) {
                    notifyItemRangeChanged(position, count);
                }

                @Override
                public boolean areContentsTheSame(FollowingLessonsContext.FollowingLessonContext oldItem, FollowingLessonsContext.FollowingLessonContext newItem) {
                    return false;
                }

                @Override
                public boolean areItemsTheSame(FollowingLessonsContext.FollowingLessonContext item1, FollowingLessonsContext.FollowingLessonContext item2) {
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
        public FollowingLessonView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new FollowingLessonView(LayoutInflater.from(parent.getContext()).inflate(R.layout.following_lesson_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull FollowingLessonView holder, int position) {
            holder.setLesson(following_lessons.get(position));
        }

        @Override
        public int getItemCount() {
            return following_lessons.size();
        }
    }
}
