package it.cnr.istc.exploraa;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.GestureDetector;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import java.util.HashSet;
import java.util.Set;

public class TeachingLessonsFragment extends Fragment {

    private RecyclerView teaching_lessons_recycler_view;
    private TeachingLessonsAdapter teaching_lessons_adapter;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        inflater.inflate(R.menu.teaching_lessons_menu, menu);
        super.onCreateOptionsMenu(menu, inflater);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.add_teaching_lesson_menu_item:
                startActivity(new Intent(getActivity(), NewLessonActivity.class));
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_teaching_lessons, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        teaching_lessons_recycler_view = view.findViewById(R.id.teaching_lessons_recycler_view);
        teaching_lessons_adapter = new TeachingLessonsAdapter(this);

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        teaching_lessons_recycler_view.setHasFixedSize(true);
        teaching_lessons_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        teaching_lessons_recycler_view.setAdapter(teaching_lessons_adapter);
    }

    @Override
    public void onResume() {
        super.onResume();
        ExPLoRAAContext.getInstance().addTeachingLessonsListener(teaching_lessons_adapter);
    }

    @Override
    public void onPause() {
        super.onPause();
        ExPLoRAAContext.getInstance().removeTeachingLessonsListener(teaching_lessons_adapter);
    }

    private static class TeachingLessonsAdapter extends RecyclerView.Adapter<TeachingLessonView> implements ExPLoRAAContext.TeachingLessonsListener {

        private TeachingLessonsFragment frgmnt;
        private Set<Integer> selected_lessons = new HashSet<>();

        private TeachingLessonsAdapter(TeachingLessonsFragment frgmnt) {
            this.frgmnt = frgmnt;
        }

        @NonNull
        @Override
        public TeachingLessonView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new TeachingLessonView(frgmnt, LayoutInflater.from(parent.getContext()).inflate(R.layout.teaching_lesson_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull TeachingLessonView holder, int position) {
            holder.setLesson(ExPLoRAAContext.getInstance().getTeachingLessons().get(position));
        }

        @Override
        public int getItemCount() {
            return ExPLoRAAContext.getInstance().getTeachingLessons().size();
        }

        @Override
        public void teachingLessonAdded(int pos, TeachingLessonContext ctx) {
            notifyItemInserted(pos);
        }

        @Override
        public void teachingLessonUpdated(int pos, TeachingLessonContext ctx) {
            notifyItemChanged(pos);
        }

        @Override
        public void teachingLessonRemoved(int pos, TeachingLessonContext ctx) {
            notifyItemRemoved(pos);
        }

        @Override
        public void teachingLessonsCleared() {
            notifyDataSetChanged();
        }
    }

    private static class TeachingLessonView extends RecyclerView.ViewHolder implements View.OnTouchListener {

        private TeachingLessonContext lesson;
        private TextView title;
        private final GestureDetector gestureDetector;

        private TeachingLessonView(final TeachingLessonsFragment frgmnt, View view) {
            super(view);
            view.setOnTouchListener(this);
            gestureDetector = new GestureDetector(frgmnt.getContext(), new GestureDetector.SimpleOnGestureListener() {
                public void onLongPress(MotionEvent e) {
                    int pos = getAdapterPosition();
                    if (frgmnt.teaching_lessons_adapter.selected_lessons.contains(pos)) {
                        frgmnt.teaching_lessons_adapter.selected_lessons.remove(pos);
                    } else {
                        frgmnt.teaching_lessons_adapter.selected_lessons.add(pos);
                    }
                    frgmnt.teaching_lessons_adapter.teachingLessonUpdated(pos, lesson);
                }

                @Override
                public boolean onDown(MotionEvent e) {
                    if (frgmnt.teaching_lessons_adapter.selected_lessons.isEmpty()) {
                        // we show the teaching lesson's details..
                        final Intent intent = new Intent(frgmnt.getContext(), TeachingLessonActivity.class);
                        intent.putExtra("lesson", lesson);
                        frgmnt.startActivity(intent);
                    } else {
                        int pos = getAdapterPosition();
                        if (frgmnt.teaching_lessons_adapter.selected_lessons.contains(pos)) {
                            frgmnt.teaching_lessons_adapter.selected_lessons.remove(pos);
                        } else {
                            frgmnt.teaching_lessons_adapter.selected_lessons.add(pos);
                        }
                        frgmnt.teaching_lessons_adapter.teachingLessonUpdated(pos, lesson);
                    }
                    return true;
                }
            });
            title = view.findViewById(R.id.teaching_lesson_name);
        }

        private void setLesson(TeachingLessonContext lesson) {
            this.lesson = lesson;
            title.setText(lesson.getLesson().name);
        }

        @Override
        public boolean onTouch(View v, MotionEvent event) {
            gestureDetector.onTouchEvent(event);
            return true;
        }
    }
}
