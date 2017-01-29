/*
 * Copyright 2017-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.util;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;

/**
 * Unit tests for {@link StreamUtils}.
 *
 * @author John Blum
 * @see java.util.stream.Stream
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.util.StreamUtils
 * @since 2.0.0
 */
public class StreamUtilsTests {

	@Test
	@SuppressWarnings("unchecked")
	public void concatNoStreams() {

		Stream<Object> stream = StreamUtils.concat();

		assertThat(stream).isNotNull();
		assertThat(stream.count()).isEqualTo(0);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void concatOneStream() {

		Stream<Integer> stream = StreamUtils.concat(Stream.of(1, 2, 3));

		assertThat(stream).isNotNull();
		assertThat(stream.collect(Collectors.toList())).containsExactly(1, 2, 3);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void concatTwoStreams() {

		Stream<Integer> stream = StreamUtils.concat(Stream.of(1, 2, 3), Stream.of(4, 5, 6));

		assertThat(stream).isNotNull();
		assertThat(stream.collect(Collectors.toList())).containsExactly(1, 2, 3, 4, 5, 6);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void concatThreeStreams() {

		Stream<Integer> stream =
			StreamUtils.concat(Stream.of(1, 2, 3), Stream.of(4, 5, 6), Stream.of(7, 8, 9));

		assertThat(stream).isNotNull();
		assertThat(stream.collect(Collectors.toList())).containsExactly(1, 2, 3, 4, 5, 6, 7, 8, 9);
	}
}
