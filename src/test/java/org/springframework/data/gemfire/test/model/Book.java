/*
 * Copyright 2018-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.test.model;

import java.io.Serializable;

import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.mapping.annotation.LuceneIndexed;
import org.springframework.data.gemfire.mapping.annotation.Region;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Abstract Data Type (ADT) modeling a {@literal Book} application domain type.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @see java.io.Serializable
 * @see org.springframework.data.gemfire.mapping.annotation.LuceneIndexed
 * @see org.springframework.data.gemfire.mapping.annotation.Region
 * @see lombok
 * @since 2.2.0
 */
@Data
@Region("Books")
@RequiredArgsConstructor(staticName = "newBook")
public class Book implements Comparable<Book>, Serializable {

	@NonNull @Id
	private final Long id;

	@NonNull @LuceneIndexed(name = "BookTitleIdx")
	private String title;

	@Override
	public int compareTo(Book other) {
		return this.getTitle().compareTo(other.getTitle());
	}

	@Override
	public String toString() {
		return this.getTitle();
	}
}
