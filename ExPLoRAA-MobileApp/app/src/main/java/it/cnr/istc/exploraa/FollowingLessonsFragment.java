package it.cnr.istc.exploraa;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
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

public class FollowingLessonsFragment extends Fragment {

    private final FollowingLessonsAdapter following_lessons_adapter = new FollowingLessonsAdapter();
    private MenuItem remove_following_lessons_menu_item;
    private BroadcastReceiver following_lesson_added_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            following_lessons_adapter.notifyItemInserted(intent.getIntExtra("position", 0));
        }
    };
    private BroadcastReceiver following_lesson_updated_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            assert getActivity() != null;
            final FollowingLessonContext lesson = ExPLoRAAContext.getInstance().getService().getFollowingLesson(intent.getLongExtra("lesson", 0));
            following_lessons_adapter.notifyItemChanged(following_lessons_adapter.lessons.indexOf(lesson));
        }
    };
    private BroadcastReceiver following_lesson_removed_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            following_lessons_adapter.notifyItemRemoved(intent.getIntExtra("position", 0));
        }
    };
    private BroadcastReceiver following_lessons_cleared_receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            following_lessons_adapter.notifyDataSetChanged();
        }
    };

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);

        assert getActivity() != null;
        getActivity().registerReceiver(following_lesson_added_receiver, new IntentFilter(ExPLoRAAService.ADDED_FOLLOWING_LESSON));
        getActivity().registerReceiver(following_lesson_updated_receiver, new IntentFilter(FollowingLessonContext.UPDATED_FOLLOWING_LESSON));
        getActivity().registerReceiver(following_lesson_removed_receiver, new IntentFilter(ExPLoRAAService.REMOVED_FOLLOWING_LESSON));
        getActivity().registerReceiver(following_lessons_cleared_receiver, new IntentFilter(ExPLoRAAService.CLEARED_FOLLOWING_LESSONS));
    }

    @Override
    public void onDestroy() {
        super.onDestroy();

        assert getActivity() != null;
        getActivity().unregisterReceiver(following_lesson_added_receiver);
        getActivity().unregisterReceiver(following_lesson_updated_receiver);
        getActivity().unregisterReceiver(following_lesson_removed_receiver);
        getActivity().unregisterReceiver(following_lessons_cleared_receiver);
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
                for (int pos : following_lessons_adapter.selected_lessons)
                    to_remove.add(c_lessons.get(pos));
                for (FollowingLessonContext ctx : to_remove)
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
        following_lessons_adapter.setLessons(lessons);
    }

    private class FollowingLessonsAdapter extends RecyclerView.Adapter<FollowingLessonView> {

        private List<FollowingLessonContext> lessons = new ArrayList<>();
        private Set<Integer> selected_lessons = new HashSet<>();

        private void setLessons(List<FollowingLessonContext> lessons) {
            this.lessons = lessons;
            notifyDataSetChanged();
        }

        @NonNull
        @Override
        public FollowingLessonView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new FollowingLessonView(LayoutInflater.from(parent.getContext()).inflate(R.layout.following_lesson_row, parent, false));
        }

        @Override
        public void onBindViewHolder(@NonNull FollowingLessonView holder, int position) {
            holder.setLesson(position, lessons.get(position));
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

        private void setLesson(int pos, FollowingLessonContext lesson) {
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
            if (following_lessons_adapter.selected_lessons.contains(pos))
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
                int pos = getAdapterPosition();
                if (following_lessons_adapter.selected_lessons.contains(pos)) {
                    following_lessons_adapter.selected_lessons.remove(pos);
                    if (following_lessons_adapter.selected_lessons.isEmpty())
                        remove_following_lessons_menu_item.setVisible(false);
                } else following_lessons_adapter.selected_lessons.add(pos);
                following_lessons_adapter.notifyItemChanged(pos);
            }
        }

        @Override
        public boolean onLongClick(View v) {
            int pos = getAdapterPosition();
            if (following_lessons_adapter.selected_lessons.contains(pos))
                following_lessons_adapter.selected_lessons.remove(pos);
            else {
                following_lessons_adapter.selected_lessons.add(pos);
                remove_following_lessons_menu_item.setVisible(true);
            }
            following_lessons_adapter.notifyItemChanged(pos);
            return true;
        }
    }
}
