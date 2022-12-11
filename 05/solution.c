#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <regex.h>

char* get_group_str(regmatch_t* group, char* source) {
    if (group->rm_so == (size_t) -1) {
        exit(EXIT_FAILURE);
    }
    char* substr = (char*) malloc((strlen(source) + 1) * sizeof(char));
    int substr_len = group->rm_eo - group->rm_so;
    strncpy(substr, source + group->rm_so, substr_len);
    substr[substr_len] = '\0';
    return substr;
}

typedef struct listnode_t {
    char val;
    struct listnode_t* next;
} listnode_t;

listnode_t* malloc_node() {
    listnode_t* node = (listnode_t*) malloc(sizeof(listnode_t));
    node->val = '\0';
    node->next = NULL;
    return node;
}

listnode_t* get_last(listnode_t* root) {
    listnode_t* current = root;
    while(current->next != NULL) {
        current = current->next;
    }
    return current;
}

listnode_t* get_before_last(listnode_t* root) {
    if (root->next == NULL) {
        return NULL;
    }
    listnode_t* before_current = root;
    listnode_t* current = root->next;
    while(current->next != NULL) {
        current = current->next;
        before_current = before_current->next;
    }
    return before_current;
}

void add_to_front(listnode_t* root, char val) {
    listnode_t* new_node = malloc_node();
    new_node->val = val;
    listnode_t* first = root->next;
    new_node->next = first;
    root->next = new_node;
}

void add_to_back(listnode_t* root, char val) {
    listnode_t* new_node = malloc_node();
    new_node->val = val;
    new_node->next = NULL;
    listnode_t* last = get_last(root);
    last->next = new_node;
}

char take_from_back(listnode_t* root) {
    listnode_t* before_last = get_before_last(root);
    if (before_last == NULL) {
        // list was empty
        return '\0';
    }
    listnode_t* last = before_last->next;
    before_last->next = NULL;
    char last_val = last->val;
    free(last);
    return last_val;
}

void print_list(listnode_t* root) {
    listnode_t* current = root;
    while ((current = current->next) != NULL) {
        printf("%c ", current->val);
    }
}

void print_state(listnode_t** column_roots, int num_cols) {
    for (int c = 0; c < num_cols; c++) {
        printf("%d | ", c + 1);
        print_list(column_roots[c]);
        printf("\n");
    }
}

void print_top_boxes(listnode_t** column_roots, int num_cols) {
    for (int c = 0; c < num_cols; c++) {
        printf("%c", get_last(column_roots[c])->val);
    }
}

int main(void) {
    FILE* fp;
    char* line = NULL;
    size_t len = 0;
    ssize_t read;

    fp = fopen("input", "r");

    int line_size = getline(&line, &len, fp);
    int num_cols = (line_size - 3) / 4 + 1;

    listnode_t* column_roots_part1[num_cols];
    listnode_t* column_roots_part2[num_cols];
    for (int c = 0; c < num_cols; c++) {
        column_roots_part1[c] = malloc_node();
        column_roots_part2[c] = malloc_node();
    }

    // parse columns
    while (line[1] != '1') {
        for (int c = 0; c < num_cols; c++) {
            int crate_index = c * 4 + 1;
            char char_at_index = line[crate_index];
            if (char_at_index != ' ') {
                add_to_front(column_roots_part1[c], char_at_index);
                add_to_front(column_roots_part2[c], char_at_index);
            }
        }
        getline(&line, &len, fp);
    }

    printf("initial state:\n");
    print_state(column_roots_part1, num_cols);

    // discard empty line
    getline(&line, &len, fp);

    regex_t regex;
    unsigned int num_groups = 4;
    if (regcomp(&regex, "move ([0-9]+) from ([0-9]+) to ([0-9]+)", REG_EXTENDED) != 0) {
        exit(EXIT_FAILURE);
    }

    // instruction lines
    while (getline(&line, &len, fp) != -1) {
        regmatch_t group_array[num_groups];
        if (regexec(&regex, line, num_groups, group_array, 0) == 0) {
            int amount = atoi(get_group_str(&group_array[1], line));
            int source = atoi(get_group_str(&group_array[2], line)) - 1;
            int dest = atoi(get_group_str(&group_array[3], line)) - 1;
            // interpret for part 1
            for (int i = 0; i < amount; i++) {
                char moving_val = take_from_back(column_roots_part1[source]);
                add_to_back(column_roots_part1[dest], moving_val);
            }
            // interpret for part 2
            char moving_group[amount];
            for (int i = 1; i <= amount; i++) {
                moving_group[amount - i] = take_from_back(column_roots_part2[source]);
            }
            for (int i = 0; i < amount; i++) {
                add_to_back(column_roots_part2[dest], moving_group[i]);
            }
        } else {
            exit(EXIT_FAILURE);
        }
    }

    printf("PART 1:\n");
    print_state(column_roots_part1, num_cols);
    printf("solution: ");
    print_top_boxes(column_roots_part1, num_cols);

    printf("\n---\n");

    printf("PART 2:\n");
    print_state(column_roots_part2, num_cols);
    printf("solution: ");
    print_top_boxes(column_roots_part2, num_cols);
    printf("\n");

    regfree(&regex);

    fclose(fp);
    if (line) {
        free(line);
    }
    exit(EXIT_SUCCESS);
}
