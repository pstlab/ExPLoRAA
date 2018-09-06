package it.cnr.istc.exploraa;

import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.util.SortedList;
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

public class FollowingLessonsFragment extends Fragment implements ExPLoRAAService.FollowingLessonsListener {

    private final FollowingLessonsAdapter following_lessons_adapter = new FollowingLessonsAdapter();
    private MenuItem remove_following_lessons_menu_item;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
        ExPLoRAAContext.getInstance().getService().addFollowingLessonsListener(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        ExPLoRAAContext.getInstance().getService().removeFollowingLessonsListener(this);
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        inflater.inflate(R.menu.following_lessons_menu, menu);
        remove_following_lessons_menu_item = menu.findItem(R.id.remove_following_lessons_menu_item);
        super.onCreateOptionsMenu(menu, inflater);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.add_following_lesson_menu_item:
                startActivity(new Intent(getActivity(), EnrollActivity.class));
                return true;
            case R.id.remove_following_lessons_menu_item:
                Collection<FollowingLessonContext> to_remove = new ArrayList<>(following_lessons_adapter.selected_lessons.size());
                final List<FollowingLessonContext> c_lessons = ExPLoRAAContext.getInstance().getService().getFollowingLessons();
                for (FollowingLessonContext ctx : following_lessons_adapter.selected_lessons)
                    ExPLoRAAContext.getInstance().getService().unfollow_lesson(ctx);
                following_lessons_adapter.selected_lessons.clear();
                remove_following_lessons_menu_item.setVisible(false);
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_following_lessons, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        RecyclerView following_lessons_recycler_view = view.findViewById(R.id.following_lessons_recycler_view);

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        following_lessons_recycler_view.setHasFixedSize(true);
        following_lessons_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        following_lessons_recycler_view.setAdapter(following_lessons_adapter);
        following_lessons_recycler_view.addItemDecoration(new DividerItemDecoration(getContext(), DividerItemDecoration.VERTICAL));
    }

    void setLessons(List<FollowingLessonContext> lessons) {
        following_lessons_adapter.lessons.addAll(lessons);
    }

    @Override
    public void followingLessonAdded(int pos, FollowingLessonContext ctx) {
        following_lessons_adapter.lessons.add(ctx);
    }

    @Override
    public void followingLessonUpdated(int pos, FollowingLessonContext ctx) {
        following_lessons_adapter.lessons.updateItemAt(indexOf(ctx), ctx);
    }

    @Override
    public void followingLessonRemoved(int pos, FollowingLessonContext ctx) {
        following_lessons_adapter.lessons.remove(ctx);
    }

    @Override
    public void followingLessonsCleared() {
        following_lessons_adapter.lessons.clear();
    }

    private int indexOf(FollowingLessonContext ctx) {
        for (int i = 0; i < following_lessons_adapter.lessons.size(); i++)
            if (ctx.equals(following_lessons_adapter.lessons.get(i)))
                return i;
        return -1;
    }

    private class FollowingLessonsAdapter extends RecyclerView.Adapter<FollowingLessonView> {

        private SortedList<FollowingLessonContext> lessons;
        private Set<FollowingLessonContext> selected_lessons = new HashSet<>();

        public FollowingLessonsAdapter() {
            this.lessons = new SortedList<>(FollowingLessonContext.class, new SortedList.Callback<FollowingLessonContext>() {
                @Override
                public int compare(FollowingLessonContext o1, FollowingLessonContext o2) {
                    return o1.getLesson().name.compareToIgnoreCase(o2.getLesson().name);
                }

                @Override
                public void onChanged(int position, int count) {
                    notifyItemRangeChanged(position, count);
                }

                @Override
                public boolean areContentsTheSame(FollowingLessonContext oldItem, FollowingLessonContext newItem) {
                    return false;
                }

                @Override
                public boolean areItemsTheSame(FollowingLessonContext item1, FollowingLessonContext item2) {
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
            holder.setLesson(lessons.get(position));
        }

        @Override
        public int getItemCount() {
            return lessons.size();
        }
    }

    private class FollowingLessonView extends RecyclerView.ViewHolder implements View.OnClickListener, View.OnLongClickListener {

        private TextView title;
        private ImageView following_lesson_status_image_view;
        private FollowingLessonContext lesson;

        private FollowingLessonView(final View view) {
            super(view);
            view.setOnClickListener(this);
            view.setOnLongClickListener(this);
            title = view.findViewById(R.id.following_lesson_name);
            following_lesson_status_image_view = view.findViewById(R.id.following_lesson_status_image_view);
        }

        private void setLesson(FollowingLessonContext lesson) {
            this.lesson = lesson;
            title.setText(lesson.getLesson().name);
            switch (lesson.getState()) {
                case Running:
                    following_lesson_status_image_view.setImageResource(R.drawable.ic_play);
                    break;
                case Paused:
                    following_lesson_status_image_view.setImageResource(R.drawable.ic_pause);
                    break;
                case Stopped:
                    following_lesson_status_image_view.setImageResource(R.drawable.ic_stop);
                    break;
            }
            if (following_lessons_adapter.selected_lessons.contains(lesson))
                itemView.setBackgroundColor(Color.LTGRAY);
            else
                itemView.setBackground(null);
        }

        @Override
        public void onClick(View v) {
            if (following_lessons_adapter.selected_lessons.isEmpty()) {
                // we show the teaching lesson's details..
                final Intent intent = new Intent(getContext(), FollowingLessonActivity.class);
                intent.putExtra("lesson_id", lesson.getLesson().id);
                startActivity(intent);
            } else {
                if (following_lessons_adapter.selected_lessons.contains(lesson)) {
                    following_lessons_adapter.selected_lessons.remove(lesson);
                    if (following_lessons_adapter.selected_lessons.isEmpty())
                        remove_following_lessons_menu_item.setVisible(false);
                } else following_lessons_adapter.selected_lessons.add(lesson);
                following_lessons_adapter.lessons.updateItemAt(following_lessons_adapter.lessons.indexOf(lesson), lesson);
            }
        }

        @Override
        public boolean onLongClick(View v) {
            if (following_lessons_adapter.selected_lessons.contains(lesson))
                following_lessons_adapter.selected_lessons.remove(lesson);
            else {
                following_lessons_adapter.selected_lessons.add(lesson);
                remove_following_lessons_menu_item.setVisible(true);
            }
            following_lessons_adapter.lessons.updateItemAt(following_lessons_adapter.lessons.indexOf(lesson), lesson);
            return true;
        }
    }
}
