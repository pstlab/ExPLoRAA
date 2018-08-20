package it.cnr.istc.exploraa;

import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.widget.DividerItemDecoration;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class TeachingLessonsFragment extends Fragment implements ExPLoRAAService.TeachingLessonsListener {

    private RecyclerView teaching_lessons_recycler_view;
    private final TeachingLessonsAdapter teaching_lessons_adapter = new TeachingLessonsAdapter();
    private MenuItem remove_teaching_lessons_menu_item;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
        ExPLoRAAContext.getInstance().getService().addTeachingLessonsListener(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        ExPLoRAAContext.getInstance().getService().removeTeachingLessonsListener(this);
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        inflater.inflate(R.menu.teaching_lessons_menu, menu);
        remove_teaching_lessons_menu_item = menu.findItem(R.id.remove_teaching_lessons_menu_item);
        super.onCreateOptionsMenu(menu, inflater);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.add_teaching_lesson_menu_item:
                startActivity(new Intent(getActivity(), NewLessonActivity.class));
                return true;
            case R.id.remove_teaching_lessons_menu_item:
                Collection<TeachingLessonContext> to_remove = new ArrayList<>(teaching_lessons_adapter.selected_lessons.size());
                final List<TeachingLessonContext> c_lessons = ExPLoRAAContext.getInstance().getService().getTeachingLessons();
                for (int pos : teaching_lessons_adapter.selected_lessons)
                    to_remove.add(c_lessons.get(pos));
                for (TeachingLessonContext ctx : to_remove)
                    ExPLoRAAContext.getInstance().getService().remove_teaching_lesson(ctx);
                teaching_lessons_adapter.selected_lessons.clear();
                remove_teaching_lessons_menu_item.setVisible(false);
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

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        teaching_lessons_recycler_view.setHasFixedSize(true);
        teaching_lessons_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        teaching_lessons_recycler_view.setAdapter(teaching_lessons_adapter);
        teaching_lessons_recycler_view.addItemDecoration(new DividerItemDecoration(getContext(), DividerItemDecoration.VERTICAL));
    }

    void setLessons(List<TeachingLessonContext> lessons) {
        teaching_lessons_adapter.setLessons(lessons);
    }

    @Override
    public void teachingLessonAdded(int pos, TeachingLessonContext ctx) {
        teaching_lessons_adapter.notifyItemInserted(pos);
    }

    @Override
    public void teachingLessonUpdated(int pos, TeachingLessonContext ctx) {
        teaching_lessons_adapter.notifyItemChanged(pos);
    }

    @Override
    public void teachingLessonRemoved(int pos, TeachingLessonContext ctx) {
        teaching_lessons_adapter.notifyItemRemoved(pos);
    }

    @Override
    public void teachingLessonsCleared() {
        teaching_lessons_adapter.notifyDataSetChanged();
    }

    private class TeachingLessonsAdapter extends RecyclerView.Adapter<TeachingLessonView> {

        private List<TeachingLessonContext> lessons = new ArrayList<>();
        private Set<Integer> selected_lessons = new HashSet<>();

        private void setLessons(List<TeachingLessonContext> lessons) {
            this.lessons = lessons;
            notifyDataSetChanged();
        }

        @NonNull
        @Override
        public TeachingLessonView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new TeachingLessonView(LayoutInflater.from(parent.getContext()).inflate(R.layout.teaching_lesson_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull TeachingLessonView holder, int position) {
            holder.setLesson(position, lessons.get(position));
        }

        @Override
        public int getItemCount() {
            return lessons.size();
        }
    }

    private class TeachingLessonView extends RecyclerView.ViewHolder implements View.OnClickListener, View.OnLongClickListener {

        private TeachingLessonContext lesson;
        private TextView title;
        private ImageView teaching_lesson_status_image_view;

        private TeachingLessonView(final View view) {
            super(view);
            view.setOnClickListener(this);
            view.setOnLongClickListener(this);
            title = view.findViewById(R.id.activity_teaching_lesson_name);
            teaching_lesson_status_image_view = view.findViewById(R.id.teaching_lesson_status_image_view);
        }

        private void setLesson(int pos, TeachingLessonContext lesson) {
            this.lesson = lesson;
            title.setText(lesson.getLesson().name);
            switch (lesson.getState()) {
                case Running:
                    teaching_lesson_status_image_view.setImageResource(R.drawable.ic_play);
                    break;
                case Paused:
                    teaching_lesson_status_image_view.setImageResource(R.drawable.ic_pause);
                    break;
                case Stopped:
                    teaching_lesson_status_image_view.setImageResource(R.drawable.ic_stop);
                    break;
            }
            if (teaching_lessons_adapter.selected_lessons.contains(pos))
                itemView.setBackgroundColor(Color.LTGRAY);
            else
                itemView.setBackground(null);
        }

        @Override
        public void onClick(View v) {
            if (teaching_lessons_adapter.selected_lessons.isEmpty()) {
                // we show the teaching lesson's details..
                final Intent intent = new Intent(getContext(), TeachingLessonActivity.class);
                intent.putExtra("lesson_id", lesson.getLesson().id);
                startActivity(intent);
            } else {
                int pos = getAdapterPosition();
                if (teaching_lessons_adapter.selected_lessons.contains(pos)) {
                    teaching_lessons_adapter.selected_lessons.remove(pos);
                    if (teaching_lessons_adapter.selected_lessons.isEmpty())
                        remove_teaching_lessons_menu_item.setVisible(false);
                } else teaching_lessons_adapter.selected_lessons.add(pos);
                teaching_lessons_adapter.notifyItemChanged(pos);
            }
        }

        @Override
        public boolean onLongClick(View v) {
            int pos = getAdapterPosition();
            if (teaching_lessons_adapter.selected_lessons.contains(pos))
                teaching_lessons_adapter.selected_lessons.remove(pos);
            else {
                teaching_lessons_adapter.selected_lessons.add(pos);
                remove_teaching_lessons_menu_item.setVisible(true);
            }
            teaching_lessons_adapter.notifyItemChanged(pos);
            return true;
        }
    }
}
