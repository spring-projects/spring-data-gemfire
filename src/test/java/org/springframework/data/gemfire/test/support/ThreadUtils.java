/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.test.support;

import java.util.concurrent.TimeUnit;

/**
 * {@link ThreadUtils} is an abstract utility class for managing Java {@link Thread Threads}.
 *
 * @author John Blum
 * @see java.lang.Thread
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public abstract class ThreadUtils {

	/* (non-Javadoc) */
	public static boolean sleep(long milliseconds) {
		try {
			Thread.sleep(milliseconds);
			return true;
		}
		catch (InterruptedException ignore) {
			Thread.currentThread().interrupt();
			return false;
		}
	}

	public static boolean timedWait(long duration) {
		return timedWait(duration, duration);
	}

	public static boolean timedWait(long duration, long interval) {
		return timedWait(duration, interval, new WaitCondition() {
			@Override public boolean waiting() {
				return true;
			}
		});
	}

	@SuppressWarnings("all")
	public static boolean timedWait(long duration, long interval, WaitCondition waitCondition) {
		final long timeout = (System.currentTimeMillis() + duration);

		interval = Math.min(interval, duration);

		try {
			while (waitCondition.waiting() && (System.currentTimeMillis() < timeout)) {
				synchronized (waitCondition) {
					TimeUnit.MILLISECONDS.timedWait(waitCondition, interval);
				}
			}
		}
		catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}

		return !waitCondition.waiting();
	}

	// TODO rename interface to Condition and waiting() method to evaluate()
	public interface WaitCondition {
		boolean waiting();
	}
}
